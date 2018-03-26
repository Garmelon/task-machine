{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TaskMachine.Database
  ( TaskRow(..)
  , initializeNewDB
  , updateTasks
  ) where

import           Control.Exception
import           Data.Maybe

import qualified Data.Text                        as T
import           Data.Time.Calendar
import qualified Database.SQLite.Simple           as DB
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.Ok        as DB
import qualified Database.SQLite.Simple.ToField   as DB

import qualified TaskMachine.DateExpr             as TM

data IntFormula = IntFormula
  { intFormulaText :: T.Text
  , intFormulaExpr :: TM.IntExpr
  }

instance DB.ToField IntFormula where
  toField = DB.toField . intFormulaText

instance DB.FromField IntFormula where
  fromField f = case DB.fromField f of
    DB.Errors e -> DB.Errors e
    DB.Ok text -> case TM.parseIntExpr (T.unpack text) of
      Nothing -> DB.Errors [] -- TODO: Proper exception?
      Just expr -> DB.Ok IntFormula{ intFormulaText = text, intFormulaExpr = expr }

data BoolFormula = BoolFormula
  { boolFormulaText :: T.Text
  , boolFormulaExpr :: TM.BoolExpr
  }

instance DB.ToField BoolFormula where
  toField = DB.toField . boolFormulaText

instance DB.FromField BoolFormula where
  fromField f = case DB.fromField f of
    DB.Errors e -> DB.Errors e
    DB.Ok text -> case TM.parseBoolExpr (T.unpack text) of
      Nothing -> DB.Errors [] -- TODO: Proper exception?
      Just expr -> DB.Ok BoolFormula{ boolFormulaText = text, boolFormulaExpr = expr }

data TaskRow = TaskRow
  { rowID               :: Integer
  , rowDeadline         :: Maybe Day
  , rowBoolFormula      :: Maybe BoolFormula -- deadline formula
  , rowIntFormula       :: Maybe IntFormula  -- info formula (e. g. age for birthdays)
  , rowDescription      :: T.Text
  , rowDetails          :: T.Text
  , rowRepetitionsTotal :: Integer
  , rowRepetitionsDone  :: Integer
  }

instance DB.ToRow TaskRow where
  toRow TaskRow{..} = DB.toRow
    ( rowID
    , rowDeadline
    , rowBoolFormula
    , rowIntFormula
    , rowDescription
    , rowDetails
    , rowRepetitionsTotal
    , rowRepetitionsDone
    )

instance DB.FromRow TaskRow where
  fromRow = do
    (a,b,c,d,e,f,g,h) <- DB.fromRow
    let rowID               = a
        rowDeadline         = b
        rowBoolFormula      = c
        rowIntFormula       = d
        rowDescription      = e
        rowDetails          = f
        rowRepetitionsTotal = g
        rowRepetitionsDone  = h
    return TaskRow{..}

-- TODO: Maybe put this in separate module and/or make less specific?
allowErrorConstraint :: IO () -> IO ()
allowErrorConstraint = handleJust isErrorConstraint (const $ return ())
  where
    isErrorConstraint DB.SQLError{DB.sqlError=DB.ErrorConstraint} = Just ()
    isErrorConstraint _                                           = Nothing

initializeNewDB :: DB.Connection -> IO ()
initializeNewDB c = do
  DB.execute_ c createTaskTable
  DB.execute_ c createVersionTable
  allowErrorConstraint $ DB.execute c fillVersionTable (DB.Only (1 :: Integer))
  where
    createTaskTable =
      "CREATE TABLE IF NOT EXISTS tasks (\
      \  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
      \  deadline TEXT,\
      \  boolFormula TEXT,\
      \  intFormula TEXT,\
      \  description TEXT NOT NULL,\
      \  details TEXT NOT NULL DEFAULT \"\",\
      \  repetitions_total INTEGER NOT NULL DEFAULT 1,\
      \  repetitions_done INTEGER NOT NULL DEFAULT 0\
      \)"
    createVersionTable =
      "CREATE TABLE IF NOT EXISTS version (\
      \  version_number INTEGER PRIMARY KEY\
      \)"
    fillVersionTable =
      "INSERT INTO version (version_number) VALUES (?)"

updateTask :: TaskRow -> Maybe TaskRow
updateTask t = undefined

updateTasks :: DB.Connection -> IO ()
updateTasks c = DB.withTransaction c $ do
  tasks <- DB.query_ c selectTasksToUpdate
  let toUpdate = catMaybes $ map updateTask tasks
      params = map (\t -> (rowDeadline t, rowID t)) toUpdate
  DB.executeMany c updateTaskRow params
  where
    selectTasksToUpdate =
      "SELECT * FROM tasks\
      \  WHERE boolFormula IS NOT NULL"
    updateTaskRow =
      "UPDATE tasks\
      \  SET deadline = ?\
      \      repetitions_done = 0\
      \  WHERE id = ?"
