{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.UI where

import           Data.Monoid

import qualified Brick             as B
import qualified Brick.Themes      as B
import qualified Graphics.Vty      as VTY

import qualified TaskMachine.Types as TM

defaultTheme :: B.Theme
defaultTheme = B.newTheme VTY.defAttr
  [ ("taskList" <> "normal",    withStyle VTY.bold $ B.fg VTY.cyan)
  , ("taskList" <> "highlight",                      B.bg VTY.cyan)
  ]
  where withStyle = flip VTY.withStyle

data ResourceName = Asdf
  deriving (Eq, Ord)

data State = State
  { sConfig :: TM.Config
  }

myApp :: B.App () () ResourceName
myApp = B.App
  { B.appDraw         = \_ -> [myTestWidget]
  , B.appHandleEvent  = B.resizeOrQuit
  , B.appStartEvent   = \s -> return s
  , B.appChooseCursor = B.neverShowCursor
  , B.appAttrMap      = const $ B.themeToAttrMap defaultTheme
  }
  where
    myTestWidget = B.withAttr ("taskList" <> "normal") (B.str "normal ") B.<+> B.withAttr ("taskList" <> "highlight") (B.str "style")
