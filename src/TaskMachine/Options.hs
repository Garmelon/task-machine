module TaskMachine.Options
  ( Options(..)
  , parseOptions
  ) where

import           Control.Applicative

import qualified Options.Applicative as O

newtype Options = Options
  { oTodofile :: FilePath
  } deriving (Show)

opts :: O.Parser Options
opts = Options <$> todofile
  where
    todofile = O.strArgument
      (  O.help "The file containing all your tasks"
      <> O.metavar "TODOFILE"
      )

optsInfo :: O.ParserInfo Options
optsInfo = O.info (opts <**> O.helper) O.fullDesc
  -- <> O.progDesc "program description"
  -- <> O.header "help header"

parseOptions :: IO Options
parseOptions = O.execParser optsInfo
