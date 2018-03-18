{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.UI
  ( defaultTheme
  ) where

import           Data.Monoid

import qualified Brick        as B
import qualified Brick.Themes as B
import qualified Graphics.Vty as VTY

defaultTheme :: B.Theme
defaultTheme = B.newTheme VTY.defAttr
  [ ("taskList"                 <> "normal",    withStyle VTY.bold $ B.fg VTY.cyan)
  , ("taskList"                 <> "highlight", withStyle VTY.bold $ B.bg VTY.cyan)
  , ("taskList" <> "urgent"     <> "normal",    withStyle VTY.bold $ B.fg VTY.yellow)
  , ("taskList" <> "urgent"     <> "highlight", withStyle VTY.bold $ B.bg VTY.yellow)
  , ("taskList" <> "veryUrgent" <> "normal",    withStyle VTY.bold $ B.fg VTY.red)
  , ("taskList" <> "veryUrgent" <> "highlight", withStyle VTY.bold $ B.bg VTY.red)
  , ("taskList" <> "overdue"    <> "normal",    withStyle VTY.bold $ B.fg VTY.magenta)
  , ("taskList" <> "overdue"    <> "highlight", withStyle VTY.bold $ B.bg VTY.magenta)
  ]
  where withStyle = flip VTY.withStyle
