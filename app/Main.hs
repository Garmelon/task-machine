{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           System.Exit

import qualified Brick.Themes        as B
import qualified Options.Applicative as O
import qualified Database.SQLite.Simple as DB

import qualified TaskMachine.UI      as TM
import qualified TaskMachine.Database as TM

-- TODO: When adding oConfigFile back, make oTaskDB a Maybe FilePath.
-- This way, it won't always overwrite the task db set in the config.
-- TODO: Add a [-c|--export-default-config CONFIGFILE] option
-- TODO: Add a [--initialize] flag to create a ~/.taskmachine/ folder and fill it with a default config and theme.
-- TODO: Have a look at other programs to see how they deal with this issue.
data Options = Options
--  { oConfigFile         :: FilePath
  { oTaskDB             :: FilePath
  , oThemePaths         :: [FilePath]
  , oExportDefaultTheme :: [FilePath]
  } deriving (Show)

argParser :: O.Parser Options
argParser = pure Options
--  <*> configFile
  <*> taskDB
  <*> many themePaths
  <*> many exportDefaultTheme
  where
--    configFile = O.strOption $ mconcat
--      [ O.short 'c'
--      , O.long "config"
--      , O.help "Specify the config file to be loaded."
--      , O.value "tasks.config"
--      , O.showDefault
--      , O.metavar "CONFIGFILE"
--      ]
    taskDB = O.strOption $ mconcat
      [ O.short 'd'
      , O.long "task-db"
      , O.help "Specify the database file where the tasks are saved."
      , O.value "~/.taskmanager/tasks.db"
      , O.showDefault
      , O.metavar "TASKDB"
      ]
    themePaths = O.strOption $ mconcat
      [ O.short 't'
      , O.long "theme"
      , O.help "Specify one or more theme files to load.\
               \ This flag can be set zero or more times."
      , O.metavar "THEMEFILE"
      ]
    exportDefaultTheme = O.strOption $ mconcat
      [ O.short 'T'
      , O.long "export-default-theme"
      , O.help "Export the application's default theme to a file.\
               \ This can be used as a starting point for a custom theme."
      , O.metavar "THEMEFILE"
      ]

argParserInfo :: O.ParserInfo Options
argParserInfo = O.info (O.helper <*> argParser) $ mconcat
  [ O.fullDesc
  ]

-- Log an action (prefixes "-> ")
action :: String -> IO ()
action = putStrLn . ("-> " ++)

-- Could probably implement using EitherT, but too lazy :)
loadThemes :: B.Theme -> [FilePath] -> IO B.Theme
loadThemes theme [] = return theme
loadThemes theme (path:paths) = do
  action $ "Loading theme " ++ show path ++ "."
  eModifiedTheme <- B.loadCustomizations path theme
  case eModifiedTheme of
    Left errMsg         -> die errMsg
    Right modifiedTheme -> loadThemes modifiedTheme paths

main :: IO ()
main = do
  options <- O.execParser argParserInfo

  -- Export default theme
  forM_ (oExportDefaultTheme options) $ \path -> do
    action $ "Exporting default theme to " ++ show path ++ "."
    B.saveTheme path TM.defaultTheme

  -- Export default config
  -- TODO

  -- Load config
  -- TODO

  -- Add command line options into config
  -- TODO

  -- According to config, load themes and connect to db
  theme <- loadThemes TM.defaultTheme $ oThemePaths options

  -- Do some debugging stuff or something
  DB.withConnection "test.db" TM.initializeNewDB

  -- Start the UI
  error "Implement UI" theme

--import qualified Database.SQLite.Simple as DB
--import qualified TaskMachine.Database as TMD
--main = DB.withConnection "test.db" TMD.initializeNewDB

--data ResourceName = Asdf
--  deriving (Eq, Ord)
--
--myApp :: B.App () () ResourceName
--myApp = B.App
--  { B.appDraw         = \_ -> [myTestWidget]
--  , B.appHandleEvent  = B.resizeOrQuit
--  , B.appStartEvent   = \s -> return s
--  , B.appChooseCursor = B.neverShowCursor
--  , B.appAttrMap      = const $ B.themeToAttrMap TM.defaultTheme
--  }
--  where
--    myTestWidget = normal B.<=> urgent B.<=> veryUrgent B.<=> overdue
--    normal     = B.withAttr ("taskList"                 <> "normal") (B.str "     normal ") B.<+> B.withAttr ("taskList"                 <> "highlight") (B.str "style")
--    urgent     = B.withAttr ("taskList" <> "urgent"     <> "normal") (B.str "     urgent ") B.<+> B.withAttr ("taskList" <> "urgent"     <> "highlight") (B.str "style")
--    veryUrgent = B.withAttr ("taskList" <> "veryUrgent" <> "normal") (B.str "very urgent ") B.<+> B.withAttr ("taskList" <> "veryUrgent" <> "highlight") (B.str "style")
--    overdue    = B.withAttr ("taskList" <> "overdue"    <> "normal") (B.str "    overdue ") B.<+> B.withAttr ("taskList" <> "overdue"    <> "highlight") (B.str "style")
