-- Copyright © 2010 Greg Weber and Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in this distribution for license information.

-- | Simplified interface for spelling suggestion.
module Text.SpellingSuggest (   
  SpellingDictionary, FlatFile, WordList,
  SpellingSuggestParameters(..),
  openDictionary, closeDictionary,
--  openWordList, openDictionaryPath,
  suggest, suggestFromList
  ) where

import Text.SpellingSuggest.Dictionary
import Text.SpellingSuggest.LowLevel
import Text.SpellingSuggest.PCDB

-- | We need to know the name and coding function
-- of each phonetic coder.
data CodingSystem = CodingSystem {
  codingSystemName :: String,
  codingSystemOp :: SpellingWordCoder }

-- | Parameters controlling the suggestion search.
data SearchParams = SearchParams {
  -- | Prefilter to use to limit search.
  searchParamsFilter :: SpellingWordFilter, 
  -- | Phonetic coder to use for matches.
  searchParamsCoder :: CodingSystem,
  -- | Maximum number of choices returned.
  searchParamsChoices :: Int }


-- | These params work OK in practice.
defaultSearchParams :: SearchParams
defaultSearchParams = SearchParams {
  searchParamsFilter = nearbyWordFilter,
  searchParamsCoder = CodingSystem {
    "phonix",
    phonix },
  searchParamsChoices = 3 }

-- | The spelling dictionary.
data SpellingDictionary = WordList [String] | 
                          FlatFile String | 
                          Database DBConnection

-- | Make a connection to the given or default database. If this fails,
-- open the given or default dictionary and cache the words.
openDictionary :: Maybe String -> Maybe String -> IO SpellingDictionary
openDictionary dbPath dictPath = do
  db <- openDB dbPath
  case db of
    Just _ -> return $ SpellingDatabase db
    Nothing -> do
      dict <- readDictionary dictPath
      return $ WordList dict

closeDictionary :: SpellingDictionary -> IO ()
closeDictionary (SpellingDatabase db) = closeDB db
closeDictionary _ = return ()

{-
openDictionaryPath :: String -> SpellingDictionary
openDictionaryPath = dictPath

openWordList :: [String] -> SpellingDictionary
openWordList = WordList
-}

-- | Suggest candidates in order using the given information. Requires
-- a valid spelling dictionary.
suggest :: SearchParams -> SpellingDictionary -> String -> IO [String]
suggest parms db word = do
  ws <- wordsFromDict db 
  return $ suggestFromList parms ws word
  where
    coder = searchParamsCoder parms
    wordsFromDict (FlatFile path) =
      readFile file >>= return . lines
    wordsFromDict (WordList ws) =
      return ws
    wordsFromDict (DatabaseFile db) = 
      let sn = codingSystemName coder
      matchDB db cn $ codingSystemOp coder word

-- | Suggest candidates from the given list in order using
-- the given information.
suggestFromList :: SearchParams -> [String] -> String -> [String]
suggestFromList parms ws word =
    take (searchParamsChoices parms) $
    tryWord (searchParamsFilter parms) op word ws
    where
      op = codingSystemOp $ searchParamsCoder parms
