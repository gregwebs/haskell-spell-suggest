--- Spelling word suggestion tool
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

-- |Create and maintain a nonvolatile database of
--  phonetic codes for cheap lookup

module Text.Spell.PCDB (
   DBConnection, filePathOfDB, defaultDictionaryForDB,
   createDB, openDB, openDBFile, matchDB, closeDB
 ) where

import qualified Control.Exception as C
import Database.SQLite
import System.IO
import Text.PhoneticCode.Soundex
import Text.PhoneticCode.Phonix

{-
{-# LANGUAGE CPP #-}
#if DEBUG
import Debug.Trace
debug a = trace (show a) a
#else
debug = id
#endif
debug :: (Show a) => a -> a
-}

filePathOfDB :: IO String
filePathOfDB = return "pcdb.sq3"

defaultDictionaryForDB :: String
defaultDictionaryForDB = "/usr/share/dict/words"

-- |Create and populate the phonetic codes database.
createDB :: String -> IO ()
createDB wfn = do
  dbfn <- filePathOfDB
  db <- openConnection dbfn
  execStatement_ db ("DROP TABLE IF EXISTS " ++ tabName tab ++ ";")
                     >>= showError
  defineTableOpt db True tab >>= showError
  wf <- openFile wfn ReadMode
  hSetEncoding wf utf8
  wc <- hGetContents wf
  let ws = lines wc
  execStatement_ db "BEGIN TRANSACTION;" >>= showError
  mapM_ (codeRow db) (ws `zip` (map (soundex True) ws `zip`
                                map phonix ws))
  execStatement_ db "COMMIT;" >>= showError
  hClose wf
  closeConnection db
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

-- | Create a connection to the default database.
openDB :: IO (Maybe DBConnection)
openDB = do
  dbfn <- filePathOfDB
  openDBFile dbfn

-- | Create a connection to the given database.
openDBFile :: String -> IO (Maybe DBConnection)
openDBFile dbfn = do
  C.catch (do db <- openReadonlyConnection dbfn
              return (Just (DBConnection db)))
          (const (return Nothing) :: C.IOException -> IO (Maybe DBConnection))

-- |Return all the words in the given coding system matching the given code.
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
