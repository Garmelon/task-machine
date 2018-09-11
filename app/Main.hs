module Main where

import           Control.Applicative
import           Control.Monad

import qualified Brick                as B
import qualified Options.Applicative  as O

import           TaskMachine.TaskList
import           TaskMachine.Todotxt
import           TaskMachine.UI

data Options = Options
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
optsInfo = O.info (opts <**> O.helper)
  (  O.fullDesc
  -- <> O.progDesc "program description"
  -- <> O.header "help header"
  )

main :: IO()
main = do
  o <- O.execParser optsInfo
  result <- loadLTasks (oTodofile o)
  case result of
    Left parseError -> putStrLn parseError
    --Right tasks -> mapM_ (putStrLn . formatTask . ltaskTask) tasks
    Right tasks -> mapM_ (print . ltaskTask) tasks
