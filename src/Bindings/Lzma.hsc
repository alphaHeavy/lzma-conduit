#include <stdio.h>
#include <string.h>
#include <bindings.dsl.h>
#include <lzma.h>

module Bindings.Lzma where
#strict_import

-- lzma_ret
#integral_t lzma_ret
#num LZMA_OK
#num LZMA_STREAM_END
#num LZMA_NO_CHECK
#num LZMA_UNSUPPORTED_CHECK
#num LZMA_GET_CHECK
#num LZMA_MEM_ERROR
#num LZMA_MEMLIMIT_ERROR
#num LZMA_FORMAT_ERROR
#num LZMA_OPTIONS_ERROR
#num LZMA_DATA_ERROR
#num LZMA_BUF_ERROR
#num LZMA_PROG_ERROR

-- lzma_action
#integral_t lzma_action
#num LZMA_RUN
#num LZMA_SYNC_FLUSH
#num LZMA_FULL_FLUSH
#num LZMA_FINISH

-- lzma_flags
#num LZMA_TELL_NO_CHECK
#num LZMA_TELL_UNSUPPORTED_CHECK
#num LZMA_TELL_ANY_CHECK
#num LZMA_CONCATENATED

-- lzma_check
#integral_t lzma_check
#num LZMA_CHECK_NONE
#num LZMA_CHECK_CRC32
#num LZMA_CHECK_CRC64
#num LZMA_CHECK_SHA256

#num LZMA_PRESET_DEFAULT
#num LZMA_PRESET_LEVEL_MASK
#num LZMA_PRESET_EXTREME

-- lzma_stream
#starttype lzma_stream
#field next_in , Ptr CUChar
#field avail_in , CSize
#field total_in , CULong
#field next_out , Ptr CUChar
#field avail_out , CSize
#field total_out , CULong
#stoptype

-- figure out what to put here?
-- #cinline LZMA_STREAM_INIT , IO <lzma_stream>

-- base.h
#ccall lzma_code , Ptr <lzma_stream> -> <lzma_action> -> IO <lzma_ret>
#ccall lzma_end , Ptr <lzma_stream> -> IO ()
#ccall lzma_memusage , Ptr <lzma_stream> -> IO CULong
#ccall lzma_memlimit_get , Ptr <lzma_stream> -> IO CULong
#ccall lzma_memlimit_set , Ptr <lzma_stream> -> CULong -> IO <lzma_ret>

-- container.h
#ccall lzma_easy_encoder_memusage , CInt -> IO CULong
#ccall lzma_easy_decoder_memusage , CInt -> IO CULong
#ccall lzma_easy_encoder , Ptr <lzma_stream> -> CInt -> <lzma_check> -> IO <lzma_ret>
#ccall lzma_auto_decoder , Ptr <lzma_stream> -> CULong -> CUInt -> IO <lzma_ret>

pokeNextIn :: Ptr C'lzma_stream -> Ptr a -> IO ()
pokeNextIn = #poke lzma_stream, next_in

pokeAvailIn :: Ptr C'lzma_stream -> CSize -> IO ()
pokeAvailIn = #poke lzma_stream, avail_in

pokeNextOut :: Ptr C'lzma_stream -> Ptr a -> IO ()
pokeNextOut = #poke lzma_stream, next_out

pokeAvailOut :: Ptr C'lzma_stream -> CSize -> IO ()
pokeAvailOut = #poke lzma_stream, avail_out

peekNextIn :: Ptr C'lzma_stream -> IO (Ptr a)
peekNextIn = #peek lzma_stream, next_in

peekAvailIn :: Ptr C'lzma_stream -> IO CSize
peekAvailIn = #peek lzma_stream, avail_in

peekNextOut :: Ptr C'lzma_stream -> IO (Ptr a)
peekNextOut = #peek lzma_stream, next_out

peekAvailOut :: Ptr C'lzma_stream -> IO CSize
peekAvailOut = #peek lzma_stream, avail_out

