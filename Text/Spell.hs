module Text.Spell ( suggest, suggestOpenDict,
  try_word, suggestFromConn, openConn,
  soundex,
  phonix,
  Dictionary(..)
  ) where

import Text.Spell.PCDB

import Data.List
import Data.Ord

import Text.EditDistance
import Text.PhoneticCode.Soundex
import Text.PhoneticCode.Phonix

import Data.Maybe (fromJust)

data Dictionary = FlatFile String | DefaultDatabase | DatabaseFile String

code :: String -> String
code word = soundex True word

-- | open dictionary and give suggestion
suggestOpenDict :: Dictionary -> String -> IO [String]
suggestOpenDict db word = do
  wordsFromDict db >>= return . (flip suggest) word
  where
    wordsFromDict :: Dictionary -> IO [String]
    wordsFromDict (FlatFile file) =
      readFile file >>= return . lines
    wordsFromDict DefaultDatabase = do
      dbfn <- filePathOfDB
      wordsFromDict (DatabaseFile dbfn)
    wordsFromDict (DatabaseFile fileName) = do
      mdb <- openDBFile fileName
      matchDB (fromJust mdb) "soundex" (code word)

-- | suggest from a list of words
suggest :: [String] -> String -> [String]
suggest dict word = try_word False id 3 word dict

-- | Fastest way to use this library
--   re-use this handle with suggestFromConn
openConn :: Dictionary -> IO DBConnection
openConn (FlatFile _) = error "use suggest or suggestOpenDict"
openConn DefaultDatabase = do
  dbfn <- filePathOfDB 
  openConn $ DatabaseFile dbfn
openConn (DatabaseFile fileName) = do
  mdb <- openDBFile fileName
  return $ fromJust mdb

-- | Fastest way to use this library
--   open a handle first with openConn
suggestFromConn :: DBConnection -> String -> IO [String]
suggestFromConn conn word = do
  dictWords <- matchDB conn "soundex" (code word)
  return $ suggest dictWords word

-- | lower level method that exposes every option
try_word :: Bool -> (String -> String) -> Int -> String -> [String] -> [String]
try_word prefilter pcode choices word =
    take choices . sortBy (comparing ed)
                 . map snd
                 . filter ((== pcode word) . fst)
                 . map sfs
                 . prefilter_f
    where
      sfs w = (pcode w, w)
      ed = edit_distance word
      prefilter_f =
          if prefilter then filter ((<= 10) . ed) else id

      edit_distance :: String -> String -> Int
      edit_distance s t =
          restrictedDamerauLevenshteinDistance ec s t where
              ec = EditCosts {
                     insertionCost = 2,
                     deletionCost = 2,
                     transpositionCost = 1,
                     substitutionCost = 3 }
