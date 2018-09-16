module Main where

import           Control.Applicative
import           Control.Monad

import qualified Brick                as B
import qualified Options.Applicative  as O

import           TaskMachine.LTask
import           TaskMachine.UI
import           TaskMachine.UI.Types

newtype Options = Options
  { oTodofile :: FilePath
  } deriving (Show)

opts :: O.Parser Options
opts = pure Options
  <*> todofile
  where
    todofile = O.strArgument
      (  O.help "The file containing all your tasks"
      <> O.metavar "TODOFILE"
      )

optsInfo :: O.ParserInfo Options
optsInfo = O.info (opts <**> O.helper) O.fullDesc
  -- <> O.progDesc "program description"
  -- <> O.header "help header"

main :: IO()
main = do
  o <- O.execParser optsInfo
  result <- loadLTasks (oTodofile o)
  case result of
    Left parseError -> putStrLn parseError
    Right tasks -> void $ B.defaultMain (myApp defaultTheme) (startUIState tasks)
