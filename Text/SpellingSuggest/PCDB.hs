-- Copyright © 2010 Greg Weber and Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in this distribution for license information.

-- | Create and maintain a nonvolatile database of
--   phonetic codes.
module Text.SpellingSuggest.PCDB (
   DBConnection, defaultDB,
   createDB, openDB, openDBFile, matchDB, closeDB
 ) where

import qualified Control.Exception as C
import Database.SQLite
import Text.PhoneticCode.Soundex
import Text.PhoneticCode.Phonix

-- | File path for default cache database.
defaultDB :: String
defaultDB = "/var/lib/spell-suggest/pcdb.sq3"

-- | Create and populate the phonetic codes database, given
-- a list of words and a database path.
createDB :: [String] -> Maybe String -> IO DBConnection
createDB ws dbPath = do
  db <- openConnection $ fromMaybe defaultDB dbPath
  execStatement_ db ("DROP TABLE IF EXISTS " ++ tabName tab ++ ";")
                     >>= showError
  defineTableOpt db True tab >>= showError
  execStatement_ db "BEGIN TRANSACTION;" >>= showError
  mapM_ (codeRow db) (ws `zip` (map (soundex True) ws `zip`
                                map phonix ws))
  execStatement_ db "COMMIT;" >>= showError
  hClose wf
  return db
    where
      codeRow db (w, (sc, pc)) =
          insertRow db (tabName tab) [(colName cw, w),
                                      (colName cs, sc),
                                      (colName cp, pc)] >>= showError

      showError :: Maybe String -> IO ()
      showError Nothing = return ()
      showError (Just s) = fail s

      --- table schema
      cw = Column { colName = "word",
                    colType = SQLVarChar 64,
                    colClauses = [ PrimaryKey False ] }
      cs = Column { colName = "soundex",
                    colType = SQLVarChar 16,
                    colClauses = [ IsNullable False ] }
      cp = Column { colName = "phonix",
                    colType = SQLVarChar 16,
                    colClauses = [ IsNullable False ] }
      tab = Table { tabName = "phonetic_codes",
                    tabColumns = [ cw, cs, cp ],
                    tabConstraints = [] }

-- | Database connection.
newtype DBConnection = DBConnection SQLiteHandle

-- | Open the phonetic codes database, given a database path.
openDB :: Maybe String -> IO (Maybe DBConnection)
openDB dbPath = 
  openDBFile $ fromMaybe defaultDB dbPath
  where
    openDBFile dbp = do
      C.catch (do db <- openReadonlyConnection dbp
               return (Just (DBConnection db)))
        (const (return Nothing) :: C.IOException -> IO (Maybe DBConnection))

-- | Return all the words in the given coding system matching the given code.
matchDB :: DBConnection -> String -> String -> IO [String]
matchDB (DBConnection db) coding code = do
    result <- execParamStatement db
      ("SELECT word FROM phonetic_codes WHERE " ++ coding ++ " = :code ;")
      [(":code", Text code)]
    case result of
      Left msg -> error msg
      Right rows -> return (map (snd . head) . head $ rows)

closeDB :: DBConnection -> IO ()
closeDB (DBConnection db) = closeConnection db
