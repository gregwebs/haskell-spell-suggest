-- Copyright © 2010 Greg Weber and Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in this distribution for license information.

-- | Simplified interface for spelling suggestion.
module Text.SpellingSuggest (   
  PhoneticCoder, SpellingDictionary(FlatFile, WordList), 
  SearchParams(..), defaultSearchParams,
  findPhoneticCoder, defaultPhoneticCoder, defaultWordFilter,
  dictionaryIsDB, openDictionary, closeDictionary,
  suggest, suggestFromList,
  nearbyWordFilter,  anyWordFilter,
  defaultDB, defaultDictionary
  ) where

import Data.List
import Text.SpellingSuggest.Dictionary
import Text.SpellingSuggest.LowLevel
import Text.SpellingSuggest.PCDB

-- | We need to know the name and coding function
-- of each phonetic coder.
data PhoneticCoder = PhoneticCoder {
  phoneticCoderName :: String,
  phoneticCoderOp :: SpellingWordCoder }

phoneticCoderPhonix :: PhoneticCoder
phoneticCoderPhonix = PhoneticCoder {
  phoneticCoderName = "phonix",
  phoneticCoderOp = phonix }

phoneticCoderSoundex :: PhoneticCoder
phoneticCoderSoundex = PhoneticCoder {
  phoneticCoderName = "soundex",
  phoneticCoderOp = soundex True }

phoneticCoders :: [PhoneticCoder]
phoneticCoders = 
  [phoneticCoderPhonix, phoneticCoderSoundex]

-- | Return the coding system with the given name.
findPhoneticCoder :: String -> Maybe PhoneticCoder
findPhoneticCoder name = 
  find ((name ==) . phoneticCoderName) phoneticCoders

-- | Parameters controlling the suggestion search.
data SearchParams = SearchParams {
  -- | Prefilter to use to limit search.
  searchParamsFilter :: SpellingWordFilter, 
  -- | Phonetic coder to use for matches.
  searchParamsCoder :: PhoneticCoder,
  -- | Maximum number of choices returned.
  searchParamsChoices :: Int }


-- | A default phonetic coder.
defaultPhoneticCoder :: PhoneticCoder
defaultPhoneticCoder = phoneticCoderPhonix

-- | A default word filter.
defaultWordFilter :: SpellingWordFilter
defaultWordFilter = nearbyWordFilter

-- | These params work OK in practice.
defaultSearchParams :: SearchParams
defaultSearchParams = SearchParams {
  searchParamsFilter = defaultWordFilter,
  searchParamsCoder = defaultPhoneticCoder,
  searchParamsChoices = 3 }

-- | The spelling dictionary.
data SpellingDictionary = WordList [String] | 
                          FlatFile String | 
                          SpellingDatabase DBConnection

-- | For performance reasons, it may sometimes be desirable
-- to know what's sitting under the dictionary.
dictionaryIsDB :: SpellingDictionary -> Bool
dictionaryIsDB (SpellingDatabase _) = True
dictionaryIsDB _ = False

-- | Make a connection to the given or default database. If this fails,
-- open the given or default dictionary and cache the words. XXX Will
-- leak a file handle if 'readDictionary' does.
openDictionary :: Maybe String -> Maybe String -> IO SpellingDictionary
openDictionary dbPath dictPath = do
  mdb <- openDB dbPath
  case mdb of
    Just db -> return $ SpellingDatabase db
    Nothing -> do
      dict <- readDictionary dictPath
      return $ WordList dict

-- | Close the connection to the given or default database.
closeDictionary :: SpellingDictionary -> IO ()
closeDictionary (SpellingDatabase db) = closeDB db
closeDictionary _ = return ()

-- | Suggest candidates in order using the given information. Requires
-- a valid spelling dictionary.
suggest :: SearchParams -> SpellingDictionary -> String -> IO [String]
suggest parms db word = do
  ws <- wordsFromDict db 
  return $ suggestFromList parms ws word
  where
    coder = searchParamsCoder parms
    wordsFromDict (FlatFile path) =
      readFile path >>= return . lines
    wordsFromDict (WordList ws) =
      return ws
    wordsFromDict (SpellingDatabase sdb) = 
      let cn = phoneticCoderName coder in
      matchDB sdb cn $ phoneticCoderOp coder word

-- | Suggest candidates from the given list in order using
-- the given information.
suggestFromList :: SearchParams -> [String] -> String -> [String]
suggestFromList parms ws word =
    take (searchParamsChoices parms) $
    tryWord (searchParamsFilter parms) op word ws
    where
      op = phoneticCoderOp $ searchParamsCoder parms
