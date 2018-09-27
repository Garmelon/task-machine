module Main where

import           Control.Monad

import qualified Brick               as B

import           TaskMachine.Options
import           TaskMachine.UI

main :: IO()
main = do
  o <- parseOptions
  state <- loadTasks $ startUIState o
  void $ B.defaultMain myApp state
