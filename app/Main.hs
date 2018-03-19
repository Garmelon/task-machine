{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           System.Exit

import qualified Brick.Themes        as B
import qualified Options.Applicative as O

import qualified TaskMachine.UI      as TM

data Options = Options
  { oConfigFile         :: FilePath
  , oTaskDB             :: FilePath
  , oThemePaths         :: [FilePath]
  , oExportDefaultTheme :: [FilePath]
  } deriving (Show)

argParser :: O.Parser Options
argParser = pure Options
  <*> configFile
  <*> taskDB
  <*> many themePaths
  <*> many exportDefaultTheme
  where
    configFile = O.strOption $ mconcat
      [ O.short 'c'
      , O.long "config"
      , O.help "Specify the config file to be loaded."
      , O.value "tasks.config"
      , O.showDefault
      , O.metavar "CONFIGFILE"
      ]
    taskDB = O.strOption $ mconcat
      [ O.short 'd'
      , O.long "task-db"
      , O.help "Specify the database file where the tasks are saved."
      , O.value "tasks.db"
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

  -- Good ol' debug print
  when True $ do
      putStrLn "- The Options -"
      print options
      putStrLn "- The End -"
      putStrLn ""

  -- Export default theme
  forM_ (oExportDefaultTheme options) $ \path -> do
    action $ "Exporting default theme to " ++ show path ++ "."
    B.saveTheme path TM.defaultTheme

  -- Load config
  -- TODO: Some config data type that contains the themes etc.

  -- Load themes and connect to db
  theme <- loadThemes TM.defaultTheme $ oThemePaths options

  -- Running the program
  error "Implement actual program logic" theme

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
