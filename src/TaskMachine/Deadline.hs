-- | Tasks and events can have one or multiple deadlines.
--
-- This module contains a representation for single deadlines,
-- and some useful functions for calculating things with them.

module TaskMachine.Deadline
  ( Deadline(..)
  , Duration
  -- * Calculations
  , relevant
  , isRelevant
  , nextDeadlines
  , relevant'
  , isRelevant'
  , nextDeadlines'
  ) where

import           Data.Maybe

import           Data.Time.Calendar

import qualified TaskMachine.DateExpr as TM

-- | Duration of a task or event in days.
--
-- A duration of 1 means one day.
-- Because of this, 'Duration' values __must always be greater than 0__.
type Duration = Int

-- | A way to represent a deadline, either as a fixed date or using a formula.
data Deadline
  = DFixed Day (Maybe Duration)
  | DExpression TM.BoolExpr Duration

-- | Find the next date of the 'Deadline' that's important for a certain day.
-- This returns a @Just@ when the day lies within the duration specified.
--
-- If no duration is specified in a 'DFixed' deadline, all days before the deadline,
-- including the deadline itself, are important (i. e. the duration is infinite).
relevant :: Day -> Deadline -> Maybe Day
relevant today (DExpression expr duration) = TM.findNext today duration expr
relevant today (DFixed day Nothing)
  | diffDays day today >= 0 = Just day
  | otherwise               = Nothing
relevant today (DFixed day (Just duration))
  | diff >= 0 && diff < toInteger duration = Just day
  | otherwise                              = Nothing
  where diff = diffDays day today

-- | A version of 'relevant' modified to take a list of Deadlines.
relevant' :: Day -> [Deadline] -> Maybe Day
relevant' today deadlines =
  let relevants = mapMaybe (relevant today) deadlines
  in  case relevants of
        []   -> Nothing
        days -> Just $ minimum days



-- | Whether the 'Deadline' is relevant on the current day or not.
--
-- This function works like 'relevant', only that the actual date calculated is irrelevant.
--
-- @'isRelevant' day = 'isJust' . 'relevant' day@
isRelevant :: Day -> Deadline -> Bool
isRelevant day = isJust . relevant day -- Hey, this even reads like English! :D

-- | A version of 'isRelevant' modified to take a list of Deadlines.
isRelevant' :: Day -> [Deadline] -> Bool
isRelevant' day = any (isRelevant day)

-- | Calculate all occurrences of this deadline within the duration given.
nextDeadlines :: Day -> Duration -> Deadline -> [Day]
nextDeadlines start duration (DFixed day _)
  | diff >= 0 && diff < toInteger duration = [day]
  | otherwise                              = []
  where diff = diffDays day start
nextDeadlines start duration (DExpression expr _) =
  TM.findWithin start duration expr

-- | A version of 'nextDeadlines' modified to take a list of Deadlines.
nextDeadlines' :: Day -> Duration -> [Deadline] -> [Day]
nextDeadlines' start duration = concatMap (nextDeadlines start duration)
