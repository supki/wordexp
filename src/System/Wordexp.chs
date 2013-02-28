{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- wordexp (and wordfree) Haskell wrappers
module System.Wordexp
 ( -- Wrapper
   wordexp
   -- Flags
 , Flags, nosubst, errors, noundef
   -- Exceptions
 , WordexpException (..)
 ) where

import Control.Exception (Exception, throw)
import Control.Monad
import Data.Data (Data)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Foreign
import Foreign.C
import Foreign.C.Types

import Data.Array (Ix)

#include <wordexp.h>


-- | Opaque wordexp_t struct image in Haskellland
data Wordexp


{# enum define FLAGS
  { WRDE_APPEND  as WRDE_APPEND
  , WRDE_DOOFFS  as WRDE_DOOFFS
  , WRDE_NOCMD   as WRDE_NOCMD
  , WRDE_REUSE   as WRDE_REUSE
  , WRDE_SHOWERR as WRDE_SHOWERR
  , WRDE_UNDEF   as WRDE_UNDEF
  } deriving (Show, Read, Eq, Ord, Bounded, Ix, Data, Typeable) #}

-- | Wordexp flags
--
-- Not every flag is supported since some of them do not make much sense in Haskell anyway
newtype Flags = Flags Int
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
  deriving (Show, Read, Eq, Ord, Bounded, Bits, Ix, Data, Typeable)
#else
  deriving (Show, Read, Eq, Ord, Bounded, Num, Bits, Ix, Data, Typeable)
#endif

instance Monoid Flags where
  mempty = Flags 0
  a `mappend` b = a .|. b
  {-# INLINE mappend #-}

-- | Disable command substitution in patterns, throw exception if found one
nosubst :: Flags
nosubst = Flags $ fromEnum WRDE_NOCMD
{-# INLINE nosubst #-}

-- | Do not hide shell error messages in /dev/null, print them right away
errors :: Flags
errors = Flags $ fromEnum WRDE_SHOWERR
{-# INLINE errors #-}

-- | Do not accept undefined shell variables, throw exception if found one
noundef :: Flags
noundef = Flags $ fromEnum WRDE_UNDEF
{-# INLINE noundef #-}


{# enum define WordexpException
  { WRDE_NOSPACE as OutOfSpace
  , WRDE_BADCHAR as IllegalCharacterOccurence
  , WRDE_BADVAL  as UndefinedShellVariable
  , WRDE_CMDSUB  as CommandSubstitution
  , WRDE_SYNTAX  as ShellSyntaxError
  } deriving (Show, Read, Eq, Ord, Bounded, Ix, Data, Typeable) #}

instance Exception WordexpException


-- | wordexp wrapper
--
-- If everything went well, return matches list otherwise throw an exception
wordexp :: String -> Flags -> IO [String]
wordexp s (Flags f) =
  withCString s $ \cs ->
    allocaBytes size $ \p -> do
      ret <- c_wordexp cs p (fromIntegral f)
      case ret of
        0 -> do
          c <- fromIntegral `fmap` wordc p
          v <- wordv p
          xs <- forM [0 .. c-1] $ peekElemOff v >=> peekCString
          c_wordfree p
          return xs
        e -> throw . (toEnum :: Int -> WordexpException) $ fromIntegral e
 where
  size =
    sizeOf (undefined :: CSize) +
    sizeOf (undefined :: Ptr CString) +
    sizeOf (undefined :: CSize)
  {-# INLINE size #-}

  wordc :: Ptr Wordexp -> IO CSize
  wordc p = peekByteOff p 0
  {-# INLINE wordc #-}

  wordv :: Ptr Wordexp -> IO (Ptr CString)
  wordv p = peekByteOff p (sizeOf (undefined :: CSize))
  {-# INLINE wordv #-}

foreign import ccall unsafe "wordexp"
  c_wordexp :: CString -> Ptr Wordexp -> CInt -> IO CInt

foreign import ccall unsafe "wordfree"
  c_wordfree :: Ptr Wordexp -> IO ()
