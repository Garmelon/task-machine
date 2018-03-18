{-# LANGUAGE OverloadedStrings #-}

module Main where

--import qualified Database.SQLite.Simple as DB
--import qualified TaskMachine.Database as TMD
--main = DB.withConnection "test.db" TMD.initializeNewDB

import           Control.Monad
import           Data.Monoid

import qualified Brick          as B
import qualified Brick.Themes   as B

import qualified TaskMachine.UI as TM

data ResourceName = Asdf
  deriving (Eq, Ord)

myApp :: B.App () () ResourceName
myApp = B.App
  { B.appDraw         = \_ -> [myTestWidget]
  , B.appHandleEvent  = B.resizeOrQuit
  , B.appStartEvent   = \s -> return s
  , B.appChooseCursor = B.neverShowCursor
  , B.appAttrMap      = const $ B.themeToAttrMap TM.defaultTheme
  }
  where
    myTestWidget = normal B.<=> urgent B.<=> veryUrgent B.<=> overdue
    normal     = B.withAttr ("taskList"                 <> "normal") (B.str "     normal ") B.<+> B.withAttr ("taskList"                 <> "highlight") (B.str "style")
    urgent     = B.withAttr ("taskList" <> "urgent"     <> "normal") (B.str "     urgent ") B.<+> B.withAttr ("taskList" <> "urgent"     <> "highlight") (B.str "style")
    veryUrgent = B.withAttr ("taskList" <> "veryUrgent" <> "normal") (B.str "very urgent ") B.<+> B.withAttr ("taskList" <> "veryUrgent" <> "highlight") (B.str "style")
    overdue    = B.withAttr ("taskList" <> "overdue"    <> "normal") (B.str "    overdue ") B.<+> B.withAttr ("taskList" <> "overdue"    <> "highlight") (B.str "style")

main :: IO ()
main = void $ B.defaultMain myApp ()
