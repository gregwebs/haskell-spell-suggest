-- Copyright © 2010 Greg Weber and Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in this distribution for license information.

-- | Read a spelling dictionary.
module Text.SpellingSuggest.Dictionary (
  defaultDictionary, readDictionary
  ) where

import Data.Maybe
import System.IO

-- | File path for default dictionary.
defaultDictionary :: String
defaultDictionary = "/usr/share/dict/words"

-- | Read the words out of the dictionary at the given path.
readDictionary :: Maybe String -> IO [String]
readDictionary dictPath = do
  wf <- flip openFile ReadMode $ fromMaybe defaultDictionary dictPath
  hSetEncoding wf utf8
  wc <- hGetContents wf
  hClose wf
  return $ lines wc
