{-# LANGUAGE RecordWildCards #-}

-- | Task related stuff.
--
-- This module will be used by both the UI and the database modules.
-- It contains some functionality independent of any of those modules.
--
-- (Although I don't really know what exactly that will be.)

module TaskMachine.Task
  ( Task(..)
  , TaskID
  ) where

import qualified Data.Text            as T

import qualified TaskMachine.DateExpr as TM
import qualified TaskMachine.Deadline as TM
import qualified TaskMachine.Subtask  as TM

type TaskID = Integer

data Task = Task
  { taskID          :: TaskID
  , taskDeadlines   :: [TM.Deadline]
  , taskFormula     :: Maybe TM.IntExpr
  , taskDescription :: T.Text
  , taskDetails     :: T.Text
  , taskSubtasks    :: [TM.Subtask]
  }

data EmptyTask = EmptyTask
  { etaskID          :: TaskID
  , etaskFormula     :: Maybe TM.IntExpr
  , etaskDescription :: T.Text
  , etaskDetails     :: T.Text
  }

{-
  ( Task(..)
  , Deadline(..)
  , fromTaskRow
  , toTaskRow
  ) where

import qualified Data.Text            as T
import           Data.Time.Calendar

import qualified TaskMachine.Database as TM

data Task = Task
  { taskID               :: TM.TaskID
  , taskDeadline         :: Deadline
  , taskIntFormula       :: Maybe TM.IntFormula
  , taskDescription      :: T.Text
  , taskDetails          :: T.Text
  , taskRepetitionsTotal :: Integer
  , taskRepetitionsDone  :: Integer
  }

data Deadline
  = DeadlineNone
  | DeadlineDay Day (Maybe TM.Duration)
  | DeadlineFormula TM.BoolFormula TM.Duration

getDeadline :: TM.TaskRow -> Deadline
getDeadline row = case TM.rowBoolFormula row of
  Just formula -> DeadlineFormula formula $ fromMaybe 1 $ TM.rowDuration row
  Nothing      -> case TM.rowDeadline row of
    Just day -> DeadlineDay day $ TM.rowDuration row
    Nothing  -> DeadlineNone

fromTaskRow :: TM.TaskRow -> Task
fromTaskRow row =
  let taskID               = TM.rowID row
      taskDeadline         = getDeadline row
      taskIntFormula       = TM.rowIntFormula row
      taskDescription      = TM.rowDescription row
      taskDetails          = TM.rowDetails row
      taskRepetitionsTotal = TM.rowRepetitionsTotal
      taskRepetitionsDone  = TM.rowRepetitionsDone
  in Task{..}

toTaskRow :: Task -> TM.TaskRow
toTaskRow task = undefined task

nextDeadline :: Day -> Deadline -> Maybe Day
updateDeadline (DeadlineFormula formula duration) =
  let expr = boolFormulaExpr formula
  in  Just $ TM.findNext expr day duration
updateDeadline _ = Nothing
-}
