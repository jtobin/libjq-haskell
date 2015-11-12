{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Libjq where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8 (useAsCString)
import Foreign
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString(..))
import System.IO.Unsafe (unsafePerformIO)

-- FIXME error handling everywhere (null pointer checks, etc)
-- FIXME haskell types in wrappers rather than C types

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

#include "jq.h"
#include "jv.h"
#include "wrapper.h"

data JV = JV deriving (Eq, Show)

instance Storable JV where
  alignment _ = #{alignment jv}
  sizeOf _    = #{size jv}

  peek _ = return JV
  poke _ = undefined

newtype JQStatePtr = JQStatePtr (Ptr JQStatePtr)

newtype JVPtr = JVPtr (Ptr JVPtr)

newtype JVParserPtr = JVParserPtr (Ptr JVParserPtr)

foreign import ccall unsafe "jq_init"
  c_jq_init :: Ptr a -> IO (Ptr JQStatePtr)

jqInit :: IO (Either String (ForeignPtr JQStatePtr))
jqInit = do
  foo <- c_jq_init nullPtr
  if   foo == nullPtr
  then return (Left "null")
  else do
    reg <- newForeignPtr finalizerFree foo
    return (Right reg)
{-# NOINLINE jqInit #-}

foreign import ccall unsafe "jq_compile"
  c_jq_compile :: Ptr JQStatePtr -> CString -> IO CInt

-- FIXME error handling
jqCompile jqsPtr json = unsafePerformIO $
  C8.useAsCString json $ \str ->
    withForeignPtr jqsPtr $ \jqs ->
      c_jq_compile jqs str

foreign import ccall unsafe "h_jq_start"
  c_jq_start :: Ptr JQStatePtr -> Ptr JVPtr -> CInt -> IO ()

jqStart jqsPtr jqvPtr flags =
  withForeignPtr jqsPtr $ \jqs ->
    withForeignPtr jqvPtr $ \jqv ->
      c_jq_start jqs jqv flags

foreign import ccall unsafe "h_jq_next"
  c_jq_next :: Ptr JQStatePtr -> IO (Ptr JVPtr)

jqNext jqsPtr = withForeignPtr jqsPtr $ \jqs ->
  alloca $ \jqv -> do
    c_jq_next jqs
    peek jqv

foreign import ccall unsafe "jv_parser_new"
  c_jv_parser_new :: CInt -> IO (Ptr JVParserPtr)

jvParserNew :: Int -> IO (ForeignPtr JVParserPtr)
jvParserNew cint = do
  ptr <- c_jv_parser_new (fromIntegral cint)
  newForeignPtr finalizerFree ptr

foreign import ccall unsafe "wrapper.h h_jv_parser_next"
  c_jv_parser_next :: Ptr JVParserPtr -> IO (Ptr JVPtr)

jvParserNext :: ForeignPtr JVParserPtr -> IO (Ptr JVPtr)
jvParserNext jvParserPtr = withForeignPtr jvParserPtr $ \jvParser ->
  alloca $ \jqv -> do
    c_jv_parser_next jvParser
    peek jqv

foreign import ccall unsafe "jv_parser_set_buf"
  c_jv_parser_set_buf :: Ptr JVParserPtr -> CString -> CInt -> CInt -> IO ()

jvParserSetBuf jvParserPtr buf l isPartial =
  C8.useAsCString buf $ \cstr ->
    withForeignPtr jvParserPtr $ \jvParser ->
      c_jv_parser_set_buf jvParser cstr (fromIntegral l) (fromIntegral isPartial)


-- test

foreign import ccall unsafe "jv_parser_next"
  c_foo :: Ptr JVParserPtr -> IO (Ptr JVPtr)

-- jvParserNexto

