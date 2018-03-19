{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.List
import           System.Exit
import           System.IO.Error

import qualified Brick.Themes           as B
import qualified Data.ConfigFile        as C
import qualified Database.SQLite.Simple as DB
import qualified Options.Applicative    as O

import qualified TaskMachine.Config     as TM
import qualified TaskMachine.Database   as TM
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

argParser :: O.Parser Options
argParser = pure Options
  <*> ((++ TM.defaultConfigFilePaths) <$> many configFile)
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
                 ++ intercalate "," (map show TM.defaultConfigFilePaths)
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

argParserInfo :: O.ParserInfo Options
argParserInfo = O.info (O.helper <*> argParser) $ mconcat
  [ O.fullDesc
  ]

-- Log an action (prefixes "-> ")
action :: String -> IO ()
action = putStrLn . ("-> " ++)

-- Kinda ugly...
loadConfigs :: [FilePath] -> IO (Maybe TM.Config)
loadConfigs [] = return Nothing
loadConfigs (path:paths) = do
  action $ "Loading config from " ++ show path ++ "."
  mConf <- tryLoadConfig path
  case mConf of
    Just conf -> return (Just conf)
    Nothing -> do
      putStrLn $ "Could not load config from " ++ show path ++ "."
      loadConfigs paths
  where
    tryLoadConfig :: FilePath -> IO (Maybe TM.Config)
    tryLoadConfig p = handleOpenFileExceptions
                    $ handleCPException
                    $ Just <$> TM.loadConfig p
    handleCPException :: IO (Maybe a) -> IO (Maybe a)
    handleCPException f = do
      res <- try f
      case res of
        Right m                                  -> return m
        Left (TM.CPException (C.ParseError msg)) -> Nothing <$ putStrLn msg
        Left (TM.CPException _                 ) -> return Nothing
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
      [] -> conf
      themes -> conf { TM.cThemes = themes }
    mergeTaskDB conf opt = case oTaskDB opt of
      Nothing -> conf
      Just taskdb -> conf { TM.cTaskDB = taskdb }

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

  -- Export default config
  forM_ (oExportDefaultConfig options) $ \path -> do
    action $ "Exporting default config to " ++ show path ++ "."
    TM.saveConfig path TM.defaultConfig

  -- Export default theme
  forM_ (oExportDefaultTheme options) $ \path -> do
    action $ "Exporting default theme to " ++ show path ++ "."
    B.saveTheme path TM.defaultTheme

  -- Load config
  mConfig <- loadConfigs $ oConfigFile options
  case mConfig of
    Nothing -> do
      putStrLn "Could not load any config."
      putStrLn "Use the -C CONFIGFILE flag to generate a default config file."
      die "No config file"
    Just unmergedConfig -> do
      -- Add command line options into config
      let config = mergeWithOptions unmergedConfig options

      -- According to config, load themes and initialize db
      theme <- loadThemes TM.defaultTheme $ TM.cThemes config
      DB.withConnection (TM.cTaskDB config) TM.initializeNewDB

      -- Start the UI
      error "Implement UI" theme config

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
