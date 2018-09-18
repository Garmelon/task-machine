{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.UI.Task
  ( renderTask
  , renderCompletion
  , renderPriority
  , renderDue
  , renderCreated
  , renderDescription
  , renderSnippet
  -- * Attributes
  , taskAttr
  , taskCompletionAttr
  , taskPriorityAttr
  , taskDueAttr
  , taskCreatedAttr
  , taskProjectAttr
  , taskContextAttr
  , taskKeyValueAttr
  ) where

import           Data.Maybe

import qualified Brick              as B
import           Data.Time.Calendar

import           TaskMachine.Task

withSpace :: B.Widget n -> B.Widget n
withSpace w = w B.<+> B.withDefAttr taskAttr (B.str " ")

renderCompletion :: Completion -> B.Widget n
renderCompletion = B.withDefAttr taskCompletionAttr . B.str . formatCompletion

renderPriority :: Priority -> B.Widget n
renderPriority p =
  let name = taskPriorityAttr <> B.attrName [priorityToChar p]
  in  B.withDefAttr name $ B.str $ formatPriority p

renderDue :: Day -> B.Widget n
renderDue = B.withDefAttr taskDueAttr . B.str . formatDue

renderCreated :: Day -> B.Widget n
renderCreated = B.withDefAttr taskCreatedAttr . B.str . formatCreated

renderDescription :: Description -> B.Widget n
renderDescription = B.withDefAttr taskAttr . B.hBox . map renderSnippet

renderSnippet :: Snippet -> B.Widget n
renderSnippet s@(Project _)    = B.withDefAttr taskProjectAttr  $ B.str $ formatSnippet s
renderSnippet s@(Context _)    = B.withDefAttr taskContextAttr  $ B.str $ formatSnippet s
renderSnippet s@(KeyValue _ _) = B.withDefAttr taskKeyValueAttr $ B.str $ formatSnippet s
renderSnippet s                =                                  B.str $ formatSnippet s

renderTask :: Task -> B.Widget n
renderTask t = B.hBox $ catMaybes
  [ Just $ withSpace $ renderCompletion $ taskCompletion t
  , (withSpace . renderPriority) <$> taskPriority t
  , (withSpace . renderDue) <$> taskDue t
  , (withSpace . renderCreated) <$> taskCreated t
  , Just $ renderDescription $ taskDescription t
  ]

taskAttr :: B.AttrName
taskAttr = "task"

taskCompletionAttr :: B.AttrName
taskCompletionAttr = taskAttr <> "completion"

taskPriorityAttr :: B.AttrName
taskPriorityAttr = taskAttr <> "priority"

taskDueAttr :: B.AttrName
taskDueAttr = taskAttr <> "due"

taskCreatedAttr :: B.AttrName
taskCreatedAttr = taskAttr <> "created"

taskProjectAttr :: B.AttrName
taskProjectAttr = taskAttr <> "project"

taskContextAttr :: B.AttrName
taskContextAttr = taskAttr <> "context"

taskKeyValueAttr :: B.AttrName
taskKeyValueAttr = taskAttr <> "keyvalue"
