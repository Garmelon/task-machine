{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.UI where

import           Data.Monoid

import qualified Brick                     as B
import qualified Brick.Themes              as B
import qualified Database.SQLite.Simple    as DB
import qualified Graphics.Vty              as VTY

import qualified TaskMachine.Config        as TM
--import qualified TaskMachine.UI.ListScreen as TM

data ResourceName = Asdf
  deriving (Eq, Ord)

data UIState = UIState
  { uiConfig       :: TM.Config
  , uiDBConnection :: DB.Connection
  , uiScreenState  :: ScreenState
  }

data ScreenState
  = Dummy
--  = ScreenList TM.ListScreen

defaultTheme :: B.Theme
defaultTheme = B.newTheme VTY.defAttr
  [ ("taskList" <> "normal",    withStyle VTY.bold $ B.fg VTY.cyan)
  , ("taskList" <> "highlight",                      B.bg VTY.cyan)
  ]
  where withStyle = flip VTY.withStyle

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
