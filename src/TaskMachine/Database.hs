{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TaskMachine.Database
  ( TaskRow(..)
  , initializeNewDB
  ) where

--import           Control.Exception

import qualified Data.Text              as T
import           Data.Time.Calendar
import qualified Database.SQLite.Simple as DB

import qualified TaskMachine.DateExpr   as TM

data TaskRow = TaskRow
  { rowID               :: Integer
  , rowDeadline         :: Maybe Day
  , rowFormula          :: Maybe TM.DateExpr
  , rowDescription      :: T.Text
  , rowDetails          :: T.Text
  , rowRepetitionsTotal :: Integer
  , rowRepetitionsDone  :: Integer
  }

instance DB.ToRow TaskRow where
  toRow TaskRow{..} = DB.toRow
    ( rowID
    , rowDeadline
    , rowFormula
    , rowDescription
    , rowDetails
    , rowRepetitionsTotal
    , rowRepetitionsDone
    )

instance DB.FromRow TaskRow where
  fromRow = do
    (a,b,c,d,e,f,g) <- DB.fromRow
    let rowID               = a
        rowDeadline         = b
        rowFormula          = c
        rowDescription      = d
        rowDetails          = e
        rowRepetitionsTotal = f
        rowRepetitionsDone  = g
    return TaskRow{..}

-- TODO: Maybe put this in separate module and/or make less specific
--allowErrorConstraint :: IO () -> IO ()
--allowErrorConstraint = handleJust isErrorConstraint (const $ return ())
--  where
--    isErrorConstraint DB.SQLError{DB.sqlError=DB.ErrorConstraint} = Just ()
--    isErrorConstraint _                                           = Nothing

initializeNewDB :: DB.Connection -> IO ()
initializeNewDB c = do
  DB.execute_ c createTaskTable
--  DB.execute_ c createVersionTable
--  allowErrorConstraint $ DB.execute c fillVersionTable (DB.Only (1 :: Integer))
  where
    createTaskTable =
      "CREATE TABLE IF NOT EXISTS tasks (\
      \  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
      \  deadline TEXT,\
      \  formula TEXT,\
      \  description TEXT NOT NULL,\
      \  details TEXT NOT NULL DEFAULT \"\",\
      \  repetitions_total INTEGER NOT NULL DEFAULT 1,\
      \  repetitions_done INTEGER NOT NULL DEFAULT 0\
      \)"
--    createVersionTable =
--      "CREATE TABLE IF NOT EXISTS version (\
--      \  version_number INTEGER PRIMARY KEY\
--      \)"
--    fillVersionTable =
--      "INSERT INTO version (version_number) VALUES (?)"
