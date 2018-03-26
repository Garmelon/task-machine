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
  , rowDuration         :: Integer -- If there is no deadline, the duration is irrelevant
  , rowBoolFormula      :: Maybe BoolFormula -- Deadline formula
  , rowIntFormula       :: Maybe IntFormula  -- Info formula (e. g. age for birthdays)
  , rowDescription      :: T.Text
  , rowDetails          :: T.Text
  , rowRepetitionsTotal :: Integer
  , rowRepetitionsDone  :: Integer
  }

instance DB.ToRow TaskRow where
  toRow TaskRow{..} = DB.toRow
    ( rowID
    , rowDeadline
    , rowDuration
    , rowBoolFormula
    , rowIntFormula
    , rowDescription
    , rowDetails
    , rowRepetitionsTotal
    , rowRepetitionsDone
    )

instance DB.FromRow TaskRow where
  fromRow = do
    (a,b,c,d,e,f,g,h,i) <- DB.fromRow
    let rowID               = a
        rowDeadline         = b
        rowDuration         = c
        rowBoolFormula      = d
        rowIntFormula       = e
        rowDescription      = f
        rowDetails          = g
        rowRepetitionsTotal = h
        rowRepetitionsDone  = i
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
      \  duration INTEGER NOT NULL,\
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

updateTask :: Day -> TaskRow -> Maybe (Day, Integer)
updateTask day t = do
  expr <- boolFormulaExpr <$> rowBoolFormula t
  nextDeadline <- TM.findNext expr day (fromIntegral $ rowDuration t)
  case rowDeadline t of
    Nothing           -> return (nextDeadline, rowID t)
    Just prevDeadline ->
      if prevDeadline == nextDeadline
        then Nothing
        else return (nextDeadline, rowID t)

updateTasks :: DB.Connection -> Day -> IO ()
updateTasks c day = DB.withTransaction c $ do
  tasks <- DB.query_ c selectTasksToUpdate
  let updated = mapMaybe (updateTask day) tasks
  DB.executeMany c updateTaskRow updated
  where
    selectTasksToUpdate =
      "SELECT * FROM tasks\
      \  WHERE boolFormula IS NOT NULL"
    updateTaskRow =
      "UPDATE tasks\
      \  SET deadline = ?\
      \      repetitions_done = 0\
      \  WHERE id = ?"
