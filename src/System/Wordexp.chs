{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | wordexp (and wordfree) Haskell wrapper
module System.Wordexp
 ( -- * Wrapper
   wordexp
   -- * Flags
 , Flags, nosubst, errors, noundef
   -- * Errors
 , WordexpError (..)
 ) where

import Control.Exception (Exception)
import Control.Monad
import Data.Data (Data)
import Data.Typeable (Typeable)
import Foreign
import Foreign.C
import Foreign.C.Types

import Data.Array (Ix)
import Data.Semigroup (Semigroup(..), Monoid(..))

#include <wordexp.h>


-- | Opaque wordexp_t struct image in Haskellland
data Wordexp


-- | wordexp flags enum image in Haskellland
{# enum define FLAGS
  { WRDE_APPEND  as WRDE_APPEND
  , WRDE_DOOFFS  as WRDE_DOOFFS
  , WRDE_NOCMD   as WRDE_NOCMD
  , WRDE_REUSE   as WRDE_REUSE
  , WRDE_SHOWERR as WRDE_SHOWERR
  , WRDE_UNDEF   as WRDE_UNDEF
  } deriving (Show, Read, Eq, Ord, Bounded, Ix, Data, Typeable) #}

-- | wordexp flags
--
-- Not every flag is supported since some of them do not make much sense in Haskell anyway
newtype Flags = Flags Int
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
  deriving (Show, Read, Eq, Ord, Bounded, Bits, Ix, Data, Typeable)
#else
  deriving (Show, Read, Eq, Ord, Bounded, Num, Bits, Ix, Data, Typeable)
#endif

instance Semigroup Flags where
  (<>) = (.|.)
  {-# INLINE (<>) #-}

instance Monoid Flags where
  mempty = Flags 0
  mappend = (<>)
  {-# INLINE mappend #-}

-- | Disable command substitution in patterns, treat them as errors
nosubst :: Flags
nosubst = Flags $ fromEnum WRDE_NOCMD
{-# INLINE nosubst #-}

-- | Do not hide shell error messages in /dev/null, print them right away
errors :: Flags
errors = Flags $ fromEnum WRDE_SHOWERR
{-# INLINE errors #-}

-- | Do not accept undefined shell variables, treat them as errors
noundef :: Flags
noundef = Flags $ fromEnum WRDE_UNDEF
{-# INLINE noundef #-}


-- | Possible wordexp errors
{# enum define WordexpError
  { WRDE_NOSPACE as OutOfSpace
  , WRDE_BADCHAR as IllegalCharacterOccurence
  , WRDE_BADVAL  as UndefinedShellVariable
  , WRDE_CMDSUB  as CommandSubstitution
  , WRDE_SYNTAX  as ShellSyntaxError
  } deriving (Show, Read, Eq, Ord, Bounded, Ix, Data, Typeable) #}

instance Exception WordexpError


-- | wordexp wrapper
--
-- Allows to specify desired flags, return expanded strings or encountered error if any
wordexp :: Flags -> String -> IO (Either WordexpError [String])
wordexp (Flags f) s =
  withCString s $ \cs ->
    allocaBytes size $ \p -> do
      ret <- c_wordexp cs p (fromIntegral f)
      case ret of
        0 -> do
          c <- fromIntegral `fmap` wordc p
          v <- wordv p
          xs <- forM [0 .. c-1] $ peekElemOff v >=> peekCString
          c_wordfree p
          return $ Right xs
        e -> return . Left . toEnum $ fromIntegral e
 where
  size = sizeOf (undefined :: CSize) + sizeOf (undefined :: Ptr CString) + sizeOf (undefined :: CSize)
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
