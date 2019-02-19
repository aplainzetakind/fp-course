{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> FilePath -> IO (List Chars)
fastAnagrams s f = map ncString . listh . S.toList . S.intersection (permutationsOfNoCaseAsSet s) . linesOfNoCaseAsSet <$> readFile f

linesOfNoCaseAsSet :: Chars -> S.Set NoCaseString
linesOfNoCaseAsSet = S.fromList . hlist . map NoCaseString . lines

permutationsOfNoCaseAsSet :: Chars -> S.Set NoCaseString
permutationsOfNoCaseAsSet = S.fromList . hlist . map NoCaseString . permutations
newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  } deriving Ord

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
