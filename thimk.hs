-- Copyright © 2010 Greg Weber and Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in this distribution for license information.

import Text.SpellingSuggest

import System.Console.ParseArgs

data ArgIndex = ArgWord
              | ArgDB
              | ArgCoder
              | ArgPrefilter
              | ArgNoPrefilter
              | ArgDict
              | ArgChoices
                deriving (Eq, Ord, Show)

main :: IO ()
main = do
  -- Parse the arguments.
  av <- parseArgsIO ArgsComplete argd
  -- Go ahead and get the dictionary opened up.
  let dictPath = getArg av ArgDict
  let dbPath = getArg av ArgDB
  dict <- openDictionary dictPath dbPath
  -- Construct the search params.
  let prefilter = prefilterate av $ dictionaryIsDB dict
  let coder =  
        case getArg av ArgCoder of
          Just codername ->
            case findPhoneticCoder codername of
              Just c -> c
              Nothing -> usageError av ("unknown phonetic coder " ++ codername)
          Nothing -> defaultPhoneticCoder
  let choices = getRequiredArg av ArgChoices
  let parms = SearchParams {
        searchParamsFilter = prefilter,
        searchParamsCoder = coder,
        searchParamsChoices = choices }
  let word = getRequiredArg av ArgWord 
  -- Do the search.
  suggestions <- suggest parms dict word
  putStr $ unlines suggestions
  where
    -- This logic for deciding when to prefilter is quite
    -- complicated and probably unnecessary.
    prefilterate av havedb =
        case npfarg && pfarg of
          True -> usageError av "prefilter schizophrenia"
          False -> 
            case not npfarg && not havedb || pfarg of
              True -> defaultWordFilter
              False -> anyWordFilter
        where
          npfarg = gotArg av ArgNoPrefilter
          pfarg = gotArg av ArgPrefilter
    -- The command-line arguments.
    argd = [ Arg { argIndex = ArgDict,
                   argName = Just "dictionary",
                   argAbbr = Just 'd',
                   argData = argDataOptional "path" ArgtypeString,
                   argDesc = "Dictionary file to search" },
             Arg { argIndex = ArgDB,
                   argName = Just "pcdb",
                   argAbbr = Just 'p',
                   argData = argDataOptional "phonetic-code-db" ArgtypeString,
                   argDesc = "Phonetic code database to search" },
             Arg { argIndex = ArgCoder,
                   argName = Just "coder",
                   argAbbr = Just 'c',
                   argData = argDataOptional "phonetic-coder" ArgtypeString,
                   argDesc = "Phonetic coder: one of soundex, phonix"},
             Arg { argIndex = ArgChoices,
                   argName = Nothing,
                   argAbbr = Just 'n',
                   argData = argDataDefaulted "choices" ArgtypeInt 1,
                   argDesc = "Max number of choices to offer"},
             Arg { argIndex = ArgPrefilter,
                   argName = Just "distance-prefilter",
                   argAbbr = Nothing,
                   argData = Nothing,
                   argDesc = "Discard wildly misspelled words early"},
             Arg { argIndex = ArgNoPrefilter,
                   argName = Just "no-distance-prefilter",
                   argAbbr = Nothing,
                   argData = Nothing,
                   argDesc = "Do not discard wildly misspelled words early"},
             Arg { argIndex = ArgWord,
                   argName = Nothing,
                   argAbbr = Nothing,
                   argData = argDataRequired "word" ArgtypeString,
                   argDesc = "Word to be looked up" } ]

