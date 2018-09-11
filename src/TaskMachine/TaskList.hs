-- | A way to store the 'Task's that preserves the original task order

module TaskMachine.TaskList
  ( LTask(..)
  ) where

import           TaskMachine.Todotxt

data LTask = LTask
  { ltaskNumber :: Integer
  -- ^ Sort by this number to get the original order of the tasks
  , ltaskTast   :: Task
  -- ^ The 'Task' itself
  }
