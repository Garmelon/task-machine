{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.UI.TaskList where

import qualified Brick                as B

import           TaskMachine.LTask
import           TaskMachine.Todotxt
import           TaskMachine.UI.Types

widgetPriority :: B.AttrName -> Maybe Priority -> B.Widget n
widgetPriority _         Nothing     = B.emptyWidget
widgetPriority highlight (Just prio) =
  let attrName = highlight <> "priority" <> B.attrName [priorityToChar prio]
      text = formatPriority prio ++ " "
  in  B.withAttr attrName $ B.str text

widgetDescription :: B.AttrName -> String -> B.Widget n
widgetDescription highlight desc =
  let attrName = highlight <> "description"
  in  B.withAttr attrName $ B.str desc

renderLTask :: Bool -> LTask -> B.Widget RName
renderLTask highlight (LTask _ Task{..}) =
  let attrHighlight = if highlight then "highlight" else "normal"
      wCompleted   = B.str $ if taskCompleted then "x " else "  "
      wPriority    = widgetPriority    attrHighlight taskPriority
      wDescription = widgetDescription attrHighlight taskDescription
  in  B.hBox [wCompleted, wPriority, wDescription]
