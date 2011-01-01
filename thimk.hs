import System.Console.ParseArgs hiding (args)
import Text.Spell
import Text.Spell.PCDB
import System.IO

data ArgIndex = ArgWord
              | ArgCode
              | ArgPrefilter
              | ArgNoPrefilter
              | ArgDict
              | ArgChoices
                deriving (Eq, Ord, Show)

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  let word = getRequiredArg args ArgWord 
  let codename = getRequiredArg args ArgCode
  let codef = pcode args codename
  let choices = getRequiredArg args ArgChoices
  mdb <- openDB
  (havedb, wordlist) <- case mdb of
    Nothing -> do
      h <- argFileOpener (getRequiredArg args ArgDict) ReadMode
      hSetEncoding h utf8
      c <- hGetContents h
      return (False, lines c)
    Just db -> do
      c <- matchDB db codename (codef word)
      return (True, c)
  let prefilter = prefilterable args havedb
  putStr . unlines $ try_word prefilter codef choices word wordlist
  where
    prefilterable args havedb =
        case npfarg && pfarg of
          True -> usageError args "prefilter schizophrenia"
          False -> not npfarg && not havedb || pfarg
        where
          npfarg = gotArg args ArgNoPrefilter
          pfarg = gotArg args ArgPrefilter
    pcode args codename =
        case codename of
          "soundex" -> soundex True
          "phonix" -> phonix
          c -> usageError args ("unknown phonetic code " ++ c)

    argd = [ Arg { argIndex = ArgDict,
                   argName = Just "dictionary",
                   argAbbr = Just 'd',
                   argData = argDataDefaulted "path" ArgtypeString
                               "/usr/share/dict/words",
                   argDesc = "Dictionary file to search" },
             Arg { argIndex = ArgCode,
                   argName = Just "code",
                   argAbbr = Just 'c',
                   argData = argDataDefaulted "phonetic-code" ArgtypeString
                               "phonix",
                   argDesc = "Phonetic code: one of soundex, phonix"},
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

