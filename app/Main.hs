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
import qualified System.Posix.User as P

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

{-
 - Exit codes
 -}

noConfigFile :: ExitCode
noConfigFile = ExitFailure 10

rethrowAs :: (Exception e, Exception f) => (e -> Maybe f) -> IO a -> IO a
rethrowAs f action = do
  res <- tryJust f action
  case res of
    Right v -> return v
    Left e  -> throwIO e

rethrowAsIf :: (Exception e, Exception f) => (e -> Bool) -> f -> IO a -> IO a
rethrowAsIf check newException action = do
  res <- tryJust (guard . check) action
  case res of
    Right v -> return v
    Left _ -> throwIO newException

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
                 ++ (intercalate "," $ map show $ TM.defaultConfigFilePaths homedir)
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
loadConfigs :: [FilePath] -> IO (Maybe TM.Config)
loadConfigs [] = return Nothing
loadConfigs (path:paths) = do
  act $ "Loading config from " ++ show path ++ "."
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
    --tryLoadConfig p = Just <$> TM.loadConfig p
    handleCPException :: IO (Maybe a) -> IO (Maybe a)
    handleCPException f = do
      res <- try f
      case res of
        Right m                                  -> return m
        Left (TM.CPException (C.ParseError msg)) -> Nothing <$ putStrLn msg
        Left (TM.CPException _                 ) -> putStrLn "Bleep" >> return Nothing
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
  mNewTheme <- tryLoadCustomizations path theme
  case mNewTheme of
    Nothing -> do
      putStrLn $ "Could not load theme from " ++ show path ++ "."
      loadThemes theme paths
    Just (Right newTheme) -> loadThemes newTheme paths
    Just (Left errMsg) -> do
      putStrLn $ "Could not load theme from " ++ show path ++ ": " ++ errMsg
      loadThemes theme paths
  where
    tryLoadCustomizations :: FilePath -> B.Theme -> IO (Maybe (Either String B.Theme))
    tryLoadCustomizations p t = handleOpenFileExceptions
                              $ Just <$> B.loadCustomizations p t
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

main :: IO ()
main = do
  homedir <- P.homeDirectory <$> (P.getUserEntryForID =<< P.getRealUserID)
  options <- O.execParser $ argParserInfo homedir

  -- Export default config
  forM_ (oExportDefaultConfig options) $ \path -> do
    act $ "Exporting default config to " ++ show path ++ "."
    TM.saveConfig path TM.defaultConfig

  -- Export default theme
  forM_ (oExportDefaultTheme options) $ \path -> do
    act $ "Exporting default theme to " ++ show path ++ "."
    B.saveTheme path TM.defaultTheme

  -- Load config
  mConfig <- loadConfigs $ oConfigFile options
  config <- case mConfig of
    Nothing -> do
      putStrLn ""
      putStrLn "Could not find any config file."
      putStrLn "Use the -C CONFIGFILE flag to generate a default config file."
      exitWith noConfigFile
    Just unmergedConfig -> return $ mergeWithOptions unmergedConfig options

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
