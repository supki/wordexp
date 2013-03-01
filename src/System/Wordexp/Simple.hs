-- | Simple wordexp (and wordfree) Haskell wrapper
module System.Wordexp.Simple
 ( -- * Simple wrapper
   wordexp
 , -- * Exceptions
   W.WordexpError (..)
 ) where

import Control.Exception (throw)
import Data.Monoid (mempty)

import qualified System.Wordexp as W


-- | Simple wordexp wrapper
--
-- Return expanded strings or throw an exception if any error is encountered
wordexp :: String -> IO [String]
wordexp s = either throw id `fmap` W.wordexp mempty s
{-# INLINE wordexp #-}
