--- Spelling word suggestion tool
--- Copyright © 2008 Bart Massey

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

--- Create a phonetic code database
--- optionally used by Text.Spell or "thimk"
import System.Console.ParseArgs
import Text.SpellingSuggest.PCDB
import Text.SpellingSuggest.Dictionary

data ArgIndex = ArgDict | ArgDB deriving (Eq, Ord, Show)

main :: IO ()
main = do
  av <- parseArgsIO ArgsComplete argd
  let dictPath = getArg av ArgDict
  dict <- readDictionary dictPath
  let dbPath = getArg av ArgDB
  db <- createDB dict dbPath
  closeDB db
  where
    argd = [ Arg { argIndex = ArgDB,
                   argName = Just "pcdb",
                   argAbbr = Just 'p',
                   argData = argDataOptional "db-path" ArgtypeString,
                   argDesc = "Database path" }, 
             Arg { argIndex = ArgDict,
                   argName = Nothing,
                   argAbbr = Nothing,
                   argData = argDataOptional "path" ArgtypeString,
                   argDesc = "Dictionary file to index" } ]
