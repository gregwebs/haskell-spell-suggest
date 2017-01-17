-- Copyright © 2010 Greg Weber and Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in this distribution for license information.

-- | Implementation-level interface for spelling suggestion.
module Text.SpellingSuggest.LowLevel (
  SpellingWordFilter, SpellingWordCoder,
  nearbyWordFilter, anyWordFilter, editDistance,
  soundex, phonix, trivialPhoneticCode,
  tryWord
  ) where

import Data.List
import Data.Ord
import Text.EditDistance
import Text.PhoneticCode.Phonix
import Text.PhoneticCode.Soundex

-- | Return 'True' if the 'editDistance' from the target word to the
--   given word is small enough.    
nearbyWordFilter :: String -> String -> Bool    
nearbyWordFilter target = (<= 10) . editDistance target
  
-- | Always returns 'True'.
anyWordFilter :: String -> String -> Bool
anyWordFilter = const (const True)

-- | The weighted edit distance between a pair of strings,
--   with weights for insertion, deletion, transposition and
--   substitution chose to try to mimic spelling errors.
editDistance :: String -> String -> Int
editDistance s t =
  restrictedDamerauLevenshteinDistance ec s t where
    ec = EditCosts {
      insertionCosts = ConstantCost 2,
      deletionCosts = ConstantCost 2,
      transpositionCosts = ConstantCost 1,
      substitutionCosts = ConstantCost 3 }

-- | Map any given word to a constant "phonetic code".
--   In other words, suppress phonetic coding.
trivialPhoneticCode :: String -> String
trivialPhoneticCode = const ""

-- | Filter function takes a word to filter for, a word
-- to compare with, and returns `True` or `False` as the
-- words are close enough to keep.
type SpellingWordFilter = String -> String -> Bool

-- | Coding function takes a word and returns its
-- phonetic code.
type SpellingWordCoder = String -> String

-- | Core algorithm for spelling suggestion. Takes a
-- prefiltering function, a phonetic coding function, a
-- limit on the number of choices returned, a target word,
-- and a list of candidate words. Returns an ordered list of
-- suggested candidates.
tryWord :: SpellingWordFilter -> SpellingWordCoder -> 
           String -> [String] -> [String]
tryWord prefilter pcode word =
  sortBy (comparing (editDistance word)) .
  map snd .
  filter ((== pcode word) . fst) .
  map sfs .
  filter (prefilter word)
    where
      sfs w = (pcode w, w)
