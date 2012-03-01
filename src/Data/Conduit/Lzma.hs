{-# LANGUAGE ScopedTypeVariables #-}

module Data.Conduit.Lzma (compress, decompress) where

import Control.Monad (forM_, liftM2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import Foreign
import Foreign.C.Types (CSize)
import System.IO.Unsafe (unsafeInterleaveIO)

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
  -> IO (Ptr C'lzma_stream)
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
    then return $! streamPtr
    else fail $ name ++ " failed: " ++ prettyRet ret

easyEncoder
  :: Maybe Int
  -> Ptr C'lzma_stream
  -> IO C'lzma_ret
easyEncoder level ptr = c'lzma_easy_encoder ptr (maybe c'LZMA_PRESET_DEFAULT fromIntegral level) c'LZMA_CHECK_CRC64

autoDecoder :: Maybe Word64 -> Ptr C'lzma_stream -> IO C'lzma_ret
autoDecoder memlimit ptr = c'lzma_auto_decoder ptr (maybe maxBound fromIntegral memlimit) 0

-- | Decompress a 'ByteString' from a lzma or xz container stream.
decompress
  :: ResourceIO m
  => Maybe Word64 -- ^ Memory limit, in bytes.
  -> Conduit ByteString m ByteString
decompress memlimit = Conduit{conduitPush = initPush, conduitClose = return []} where
  initPush input = do
    (_, streamPtr) <- withIO
      (initStream "lzma_auto_decoder" (autoDecoder memlimit))
      (\ ptr -> c'lzma_end ptr >> free ptr)
    lzmaPush streamPtr input

-- | Compress a 'ByteString' into a xz container stream.
compress
  :: ResourceIO m
  => Maybe Int -- ^ Compression level from [0..9], defaults to 6.
  -> Conduit ByteString m ByteString
compress level = Conduit{conduitPush = initPush, conduitClose = return []} where
  initPush input = do
    (_, streamPtr) <- withIO
      (initStream "lzma_easy_encoder" (easyEncoder level))
      (\ ptr -> c'lzma_end ptr >> free ptr)
    lzmaPush streamPtr input

lzmaConduit
  :: ResourceIO m
  => Ptr C'lzma_stream
  -> Conduit ByteString m ByteString
lzmaConduit =
  liftM2 Conduit lzmaPush lzmaClose

lzmaPush
  :: ResourceIO m
  => Ptr C'lzma_stream
  -> ByteString
  -> ResourceT m (ConduitResult ByteString m ByteString)
lzmaPush streamPtr xs = do
  chunks <- liftIO $ codeEnum streamPtr xs
  return $! Producing (lzmaConduit streamPtr) chunks

lzmaClose
  :: Control.Monad.IO.Class.MonadIO m
  => Ptr C'lzma_stream
  -> m [ByteString]
lzmaClose streamPtr = liftIO $
  buildChunks streamPtr c'LZMA_FINISH c'LZMA_OK

codeEnum
  :: Ptr C'lzma_stream
  -> ByteString
  -> IO [ByteString]
codeEnum streamPtr chunk =
  B.unsafeUseAsCStringLen chunk $ \ (ptr, len) -> do
    pokeNextIn streamPtr ptr
    pokeAvailIn streamPtr $ fromIntegral len
    buildChunks streamPtr c'LZMA_RUN c'LZMA_OK

buildChunks
  :: Ptr C'lzma_stream
  -> C'lzma_action
  -> C'lzma_ret
  -> IO [B.ByteString]
buildChunks streamPtr action status = do
  availIn <- peekAvailIn streamPtr
  availOut <- peekAvailOut streamPtr
  codeStep streamPtr action status availIn availOut

codeStep
  :: Ptr C'lzma_stream
  -> C'lzma_action
  -> C'lzma_ret
  -> CSize
  -> CSize
  -> IO [B.ByteString]
codeStep streamPtr action status availIn availOut
  -- the inner enumerator has finished and we're done flushing the coder
  | availOut == bufferSize && status == c'LZMA_STREAM_END =
      return []

  -- the normal case, we have some results..
  | availOut < bufferSize = do
      x <- getChunk streamPtr availOut
      if availIn == 0 -- no more input, stop processing
        then return $! [x]
        else do
          -- run lzma_code forward just far enough to read all the input buffer
          xs <- unsafeInterleaveIO $ buildChunks streamPtr action status
          return $! x:xs

  -- the input buffer points into a pinned bytestring, so we need to make sure it's been
  -- fully loaded (availIn == 0) before returning
  | availIn > 0 || action == c'LZMA_FINISH = do
      ret <- c'lzma_code streamPtr action
      if ret == c'LZMA_OK || ret == c'LZMA_STREAM_END
        then buildChunks streamPtr action ret
        else fail $ "lzma_code failed: " ++ prettyRet ret

  -- nothing to do here
  | otherwise =
      return []

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
      return $! bs

  | otherwise =
      return $! B.empty

