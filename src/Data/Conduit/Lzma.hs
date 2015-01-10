{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Conduit.Lzma (compress, decompress) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString(PS))
import Data.Conduit
import Foreign
import Foreign.C.Types (CSize, CUChar)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import Bindings.Lzma

prettyRet
  :: C'lzma_ret
  -> String
prettyRet r
  | r == c'LZMA_OK                = "Operation completed successfully"
  | r == c'LZMA_STREAM_END        = "End of stream was reached"
  | r == c'LZMA_NO_CHECK          = "Input stream has no integrity check"
  | r == c'LZMA_UNSUPPORTED_CHECK = "Cannot calculate the integrity check"
  | r == c'LZMA_GET_CHECK         = "Integrity check type is now available"
  | r == c'LZMA_MEM_ERROR         = "Cannot allocate memory"
  | r == c'LZMA_MEMLIMIT_ERROR    = "Memory usage limit was reached"
  | r == c'LZMA_FORMAT_ERROR      = "File format not recognized"
  | r == c'LZMA_OPTIONS_ERROR     = "Invalid or unsupported options"
  | r == c'LZMA_DATA_ERROR        = "Data is corrupt"
  | r == c'LZMA_BUF_ERROR         = "No progress is possible"
  | r == c'LZMA_PROG_ERROR        = "Programming error"
  | otherwise                     = "Unknown LZMA error: "++show r

bufferSize
  :: Num a => a
bufferSize = 4096

memset
  :: forall a . Storable a
  => Ptr a
  -> Word8
  -> IO ()
memset ptr val =
  forM_ [0..sizeOf (undefined :: a) - 1] $ \ i ->
    pokeByteOff ptr i val

initStream
  :: String
  -> (Ptr C'lzma_stream -> IO C'lzma_ret)
  -> IO (Ptr C'lzma_stream, Ptr CUChar)
initStream name fun = do
  buffer <- mallocBytes bufferSize
  streamPtr <- malloc
  memset streamPtr 0
  poke streamPtr C'lzma_stream
    { c'lzma_stream'next_in   = nullPtr
    , c'lzma_stream'avail_in  = 0
    , c'lzma_stream'total_in  = 0
    , c'lzma_stream'next_out  = buffer
    , c'lzma_stream'avail_out = bufferSize
    , c'lzma_stream'total_out = 0 }
  ret <- fun streamPtr
  if ret == c'LZMA_OK
    then return (streamPtr, buffer)
    else fail $ name ++ " failed: " ++ prettyRet ret

easyEncoder
  :: Maybe Int
  -> Ptr C'lzma_stream
  -> IO C'lzma_ret
easyEncoder level ptr =
  let level' = maybe c'LZMA_PRESET_DEFAULT fromIntegral level
  in c'lzma_easy_encoder ptr level' c'LZMA_CHECK_CRC64

autoDecoder
  :: Maybe Word64
  -> Ptr C'lzma_stream
  -> IO C'lzma_ret
autoDecoder memlimit ptr =
  c'lzma_auto_decoder ptr (maybe maxBound fromIntegral memlimit) c'LZMA_CONCATENATED

-- | Decompress a 'ByteString' from a lzma or xz container stream.
decompress
  :: (MonadResource m)
  => Maybe Word64 -- ^ Memory limit, in bytes.
  -> Conduit ByteString m ByteString
decompress memlimit = do
  mval <- await
  case mval of
    Just input -> do
      (streamKey, (streamPtr, _)) <- lift $ allocate
        (initStream "lzma_auto_decoder" (autoDecoder memlimit))
        (\ (streamPtr, buffer) -> c'lzma_end streamPtr >> free streamPtr >> free buffer)
      codeEnum streamKey streamPtr input

    Nothing -> return ()

-- | Compress a 'ByteString' into a xz container stream.
compress
  :: (MonadResource m)
  => Maybe Int -- ^ Compression level from [0..9], defaults to 6.
  -> Conduit ByteString m ByteString
compress level = do
  mval <- await
  case mval of
    Just input -> do
      (streamKey, (streamPtr, _)) <- lift $ allocate
        (initStream "lzma_easy_encoder" (easyEncoder level))
        (\ (streamPtr, buffer) -> c'lzma_end streamPtr >> free streamPtr >> free buffer)
      codeEnum streamKey streamPtr input

    Nothing -> return ()

lzmaConduit
  :: (MonadResource m)
  => ReleaseKey
  -> Ptr C'lzma_stream
  -> Conduit ByteString m ByteString
lzmaConduit streamKey streamPtr = do
  mval <- await
  case mval of
    Just val -> codeEnum streamKey streamPtr val
    Nothing  -> lzmaClose streamKey streamPtr

lzmaClose
  :: (MonadResource m)
  => ReleaseKey
  -> Ptr C'lzma_stream
  -> Conduit ByteString m ByteString
lzmaClose streamKey streamPtr = do
  inputKey <- lift . register $ return ()
  buildChunks streamKey inputKey streamPtr c'LZMA_FINISH c'LZMA_OK

codeEnum
  :: (MonadResource m)
  => ReleaseKey
  -> Ptr C'lzma_stream
  -> ByteString
  -> Conduit B.ByteString m B.ByteString
codeEnum streamKey streamPtr chunk@(PS fptr _ _) = do
  liftIO $ do
    -- let the bytestring library calculate the chunk length
    (ptr, len) <- B.unsafeUseAsCStringLen chunk return
    pokeNextIn streamPtr ptr
    pokeAvailIn streamPtr $ fromIntegral len

  inputKey <- lift . register $ touchForeignPtr fptr

  buildChunks streamKey inputKey streamPtr c'LZMA_RUN c'LZMA_OK

buildChunks
  :: (MonadResource m)
  => ReleaseKey
  -> ReleaseKey
  -> Ptr C'lzma_stream
  -> C'lzma_action
  -> C'lzma_ret
  -> Conduit B.ByteString m B.ByteString
buildChunks streamKey inputKey streamPtr action status = do
  availIn <- liftIO $ peekAvailIn streamPtr
  availOut <- liftIO $ peekAvailOut streamPtr
  codeStep streamKey inputKey streamPtr action status availIn availOut

codeStep
  :: (MonadResource m)
  => ReleaseKey
  -> ReleaseKey
  -> Ptr C'lzma_stream
  -> C'lzma_action
  -> C'lzma_ret
  -> CSize
  -> CSize
  -> Conduit B.ByteString m B.ByteString
codeStep streamKey inputKey streamPtr action status availIn availOut
  -- the inner enumerator has finished and we're done flushing the coder
  | availOut == bufferSize && status == c'LZMA_STREAM_END = do
      lift $ do
        release inputKey
        release streamKey
      return ()

  -- the normal case, we have some results..
  | availOut < bufferSize = do
      x <- liftIO $ getChunk streamPtr availOut
      yield x
      buildChunks streamKey inputKey streamPtr action status

  -- the input buffer points into a pinned bytestring, so we need to make sure it's been
  -- fully loaded (availIn == 0) before returning
  | availIn > 0 || action == c'LZMA_FINISH = do
      ret <- liftIO $ c'lzma_code streamPtr action
      if ret == c'LZMA_OK || ret == c'LZMA_STREAM_END
        then buildChunks streamKey inputKey streamPtr action ret
        else fail $ "lzma_code failed: " ++ prettyRet ret

  -- nothing to do here
  | otherwise = do
      lift $ release inputKey
      lzmaConduit streamKey streamPtr

getChunk
  :: Ptr C'lzma_stream
  -> CSize
  -> IO B.ByteString
getChunk streamPtr availOut
  | availOut < bufferSize = do
      nextOut <- peekNextOut streamPtr
      let avail = bufferSize - fromIntegral availOut
          baseBuffer = nextOut `plusPtr` (-avail)
      bs <- B.packCStringLen (baseBuffer, avail)
      pokeAvailOut streamPtr bufferSize
      -- B.pack* copies the buffer, so reuse it
      pokeNextOut streamPtr baseBuffer
      return bs

  | otherwise =
      return B.empty
