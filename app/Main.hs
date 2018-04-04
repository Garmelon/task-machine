{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.List
--import           Data.Maybe
import           System.Exit
import           System.IO.Error

import qualified Brick.Themes           as B
import qualified Data.ConfigFile        as C
--import qualified Data.Text              as T
--import           Data.Time.Calendar
--import           Data.Time.Clock
import qualified Database.SQLite.Simple as DB
import qualified Options.Applicative    as O
import qualified System.Posix.User      as P

import qualified TaskMachine.Config     as TM
--import qualified TaskMachine.Database   as TM
--import qualified TaskMachine.DateExpr   as TM
import qualified TaskMachine.UI         as TM

-- TODO: When adding oConfigFile back, make oTaskDB a Maybe FilePath.
-- This way, it won't always overwrite the task db set in the config.
-- TODO: Add a [-c|--export-default-config CONFIGFILE] option
-- TODO: Add a [--initialize] flag to create a ~/.taskmachine/ folder and fill it with a default config and theme.
-- TODO: Have a look at other programs to see how they deal with this issue.
data Options = Options
  { oConfigFile          :: [FilePath]
  , oTaskDB              :: Maybe FilePath
  , oThemePaths          :: [FilePath]
  , oExportDefaultConfig :: [FilePath]
  , oExportDefaultTheme  :: [FilePath]
  } deriving (Show)

{-
 - Exit codes
 -}

noConfigFile :: ExitCode
noConfigFile = ExitFailure 10

{-
 - Useful functions
 -}

-- Log an action (prefixes "-> ")
act :: String -> IO ()
act = putStrLn . ("-> " ++)

{-
 - Command line options
 -}

argParser :: FilePath -> O.Parser Options
argParser homedir = pure Options
  <*> ((++ TM.defaultConfigFilePaths homedir) <$> many configFile)
  <*> optional taskDB
  <*> many themePaths
  <*> many exportDefaultConfig
  <*> many exportDefaultTheme
  where
    configFile = O.strOption $ mconcat
      [ O.short 'c'
      , O.long "config"
      , O.help $ "Where to look for a config file.\
                 \ This option can be set multiple times.\
                 \ If not specified, will look in these locations:\n"
                 ++ intercalate "," (map show $ TM.defaultConfigFilePaths homedir)
      , O.metavar "CONFIGFILE"
      ]
    taskDB = O.strOption $ mconcat
      [ O.short 'd'
      , O.long "task-db"
      , O.help "Specify the database file where the tasks are saved.\
               \ This option overwrites the config file."
      , O.metavar "TASKDB"
      ]
    themePaths = O.strOption $ mconcat
      [ O.short 't'
      , O.long "theme"
      , O.help "Specify one or more theme files to load.\
               \ This option can be set zero or more times.\
               \ This option overwrites the config file."
      , O.metavar "THEMEFILE"
      ]
    exportDefaultConfig = O.strOption $ mconcat
      [ O.short 'C'
      , O.long "export-default-config"
      , O.help "Export the application's default config to a file."
      , O.metavar "CONFIGFILE"
      ]
    exportDefaultTheme = O.strOption $ mconcat
      [ O.short 'T'
      , O.long "export-default-theme"
      , O.help "Export the application's default theme to a file.\
               \ This can be used as a starting point for a custom theme."
      , O.metavar "THEMEFILE"
      ]

argParserInfo :: FilePath -> O.ParserInfo Options
argParserInfo homedir = O.info (O.helper <*> argParser homedir) $ mconcat
  [ O.fullDesc
  ]

{-
 - Loading config and stuff
 -}

-- Kinda ugly...
loadConfigs :: TM.HomeDir -> [FilePath] -> IO (Maybe TM.Config)
loadConfigs _ [] = return Nothing
loadConfigs homedir (path:paths) = do
  act $ "Loading config from " ++ show path ++ "."
  mConf <- handleOpenFileExceptions $ tryLoadConfig path
  case mConf of
    Just conf -> return (Just conf)
    Nothing -> do
      putStrLn $ "Could not load config from " ++ show path ++ "."
      loadConfigs homedir paths
  where
    tryLoadConfig :: FilePath -> IO (Maybe TM.Config)
    tryLoadConfig p = do
      eConf <- TM.loadConfig homedir p
      case eConf of
        Right conf              -> return $ Just conf
        Left (C.ParseError msg) -> Nothing <$ putStrLn msg
        Left _                  -> return Nothing
    handleOpenFileExceptions :: IO (Maybe a) -> IO (Maybe a)
    handleOpenFileExceptions f = do
      res <- tryJust (guard . isRelevantError) f
      case res of
        Right m -> return m
        Left _  -> return Nothing
    isRelevantError :: IOError -> Bool
    isRelevantError e =  isAlreadyInUseError e
                      || isDoesNotExistError e
                      || isPermissionError e

mergeWithOptions :: TM.Config -> Options -> TM.Config
mergeWithOptions = mergeThemePaths <=< mergeTaskDB
  where
    mergeThemePaths conf opt = case oThemePaths opt of
      []     -> conf
      themes -> conf { TM.cThemes = themes }
    mergeTaskDB conf opt = case oTaskDB opt of
      Nothing     -> conf
      Just taskdb -> conf { TM.cTaskDB = taskdb }

-- Could probably implement using EitherT, but too lazy :)
loadThemes :: B.Theme -> [FilePath] -> IO B.Theme
loadThemes theme [] = return theme
loadThemes theme (path:paths) = do
  act $ "Loading theme from " ++ show path ++ "."
  mNewTheme <- handleOpenFileExceptions $ B.loadCustomizations path theme
  case mNewTheme of
    Nothing -> do
      putStrLn $ "Could not load theme from " ++ show path ++ "."
      loadThemes theme paths
    Just (Right newTheme) -> loadThemes newTheme paths
    Just (Left errMsg) -> do
      putStrLn $ "Could not load theme from " ++ show path ++ ": " ++ errMsg
      loadThemes theme paths
  where
    handleOpenFileExceptions :: IO a -> IO (Maybe a)
    handleOpenFileExceptions f = do
      res <- tryJust (guard . isRelevantError) f
      case res of
        Right m -> return $ Just m
        Left _  -> return Nothing
    isRelevantError :: IOError -> Bool
    isRelevantError e =  isAlreadyInUseError e
                      || isDoesNotExistError e
                      || isPermissionError e

main :: IO ()
main = do
  homedir <- P.homeDirectory <$> (P.getUserEntryForID =<< P.getRealUserID)
  options <- O.execParser $ argParserInfo homedir

  -- Export default config
  forM_ (oExportDefaultConfig options) $ \path -> do
    act $ "Exporting default config to " ++ show path ++ "."
    TM.saveConfig path $ TM.defaultConfig homedir

  -- Export default theme
  forM_ (oExportDefaultTheme options) $ \path -> do
    act $ "Exporting default theme to " ++ show path ++ "."
    B.saveTheme path TM.defaultTheme

  -- Load config
  mConfig <- loadConfigs homedir $ oConfigFile options
  config <- case mConfig of
    Nothing -> do
      putStrLn ""
      putStrLn "Could not find any config file."
      putStrLn "Use the -C CONFIGFILE flag to generate a default config file."
      exitWith noConfigFile
    Just unmergedConfig -> return $ mergeWithOptions unmergedConfig options

  -- According to config, load themes
  theme <- loadThemes TM.defaultTheme $ TM.cThemes config

  -- ... and initialize db
  act $ "Using db at " ++ show (TM.cTaskDB config) ++ "."
  DB.withConnection (TM.cTaskDB config) $ \c -> do
    --TM.initializeNewDB c

    -- TESTING
    testDB c

    -- Start the UI
    error "Implement UI" theme config

testDB :: DB.Connection -> IO ()
testDB _ = do
  {-
  now <- utctDay <$> getCurrentTime
  let deadlineBefore = Just $ addDays (-2) now
      deadlineAfter = Just $ addDays 2 now
      boolFormulaText = "weekday == tue && monthcount == 1"
      boolFormulaExpr = fromJust $ TM.parseBoolExpr boolFormulaText
      boolFormula = Just $ TM.BoolFormula (T.pack boolFormulaText) boolFormulaExpr
      duration = 10
      taskOne = TM.TaskRow 0 deadlineBefore duration Nothing Nothing "task 1" "" 1 0
      taskTwo = TM.TaskRow 0 deadlineAfter duration Nothing Nothing "task 2" "" 1 0
      taskThree = TM.TaskRow 0 deadlineBefore duration boolFormula Nothing "task 3" "" 1 1
      taskFour = TM.TaskRow 0 deadlineAfter duration boolFormula Nothing "task 4" "" 0 0
      taskFive = TM.TaskRow 0 Nothing duration boolFormula Nothing "task 5" "" 1 0
  mapM_ (TM.addTask c) [taskOne, taskTwo, taskThree, taskFour, taskFive]
  TM.updateTasks c now
  putStrLn "RELEVANT TASKS"
  tasks <- TM.selectRelevantTasks c now
  forM_ tasks $ print . TM.rowDescription
  putStrLn "DOIN A TASK"
  TM.doTask c $ TM.rowID $ head tasks
  putStrLn "DELETIN A TASK"
  TM.removeTask c $ TM.rowID $ tasks !! 1
  putStrLn "RELEVANT TASKS"
  tasks2 <- TM.selectRelevantTasks c now
  forM_ tasks2 $ print . TM.rowDescription
  -}
  putStrLn "Everything works (because there's nothing here...)"
