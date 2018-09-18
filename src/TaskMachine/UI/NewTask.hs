module TaskMachine.UI.NewTask where

import qualified Brick                as B

import           TaskMachine.UI.Types

placeholderNewTask :: B.Widget RName
placeholderNewTask = B.str "New: " B.<+> B.vLimit 1 (B.fill '_')
