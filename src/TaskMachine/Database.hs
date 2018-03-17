{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.Database
  ( initializeNewDB
  ) where

--import qualified Data.Text              as T
import qualified Database.SQLite.Simple as DB

initializeNewDB :: DB.Connection -> IO ()
initializeNewDB c = do
  DB.execute_ c createTaskTable
  DB.execute_ c createVersionTable
  DB.execute c fillVersionTable (DB.Only (1 :: Integer))
  where
    createTaskTable =
      "CREATE TABLE IF NOT EXISTS tasks (\
      \  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
      \  deadline TEXT,\
      \  formula TEXT,\
      \  description TEXT NOT NULL,\
      \  repetitions_total INTEGER NOT NULL DEFAULT 1,\
      \  repetitions_done INTEGER NOT NULL DEFAULT 0\
      \)"
    createVersionTable =
      "CREATE TABLE version (\
      \  version_number INTEGER\
      \)"
    fillVersionTable =
      "INSERT INTO version (version_number) VALUES (?)"
