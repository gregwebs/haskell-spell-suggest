--- Spelling word suggestion tool
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

--- Create a phonetic code database
--- optionally used by Text.Spell or "thimk"
import System.Console.ParseArgs hiding (args)
import Text.Spell.PCDB

data ArgIndex = ArgDict deriving (Eq, Ord, Show)

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  createDB (getRequiredArg args ArgDict)
  where
    argd = [ Arg { argIndex = ArgDict,
                   argName = Nothing,
                   argAbbr = Nothing,
                   argData = argDataDefaulted "path"
                                              ArgtypeString
                                              defaultDictionaryForDB,
                   argDesc = "Dictionary file to index" } ]
