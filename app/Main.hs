{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           System.Exit

import qualified Brick.Themes        as B
import qualified Options.Applicative as O

import qualified TaskMachine.UI      as TM

data Options = Options
  { oThemePaths         :: [FilePath]
  , oExportDefaultTheme :: [String]
  } deriving (Show)

argParser :: O.Parser Options
argParser = pure Options
  <*> many themePaths
  <*> many exportDefaultTheme
  where
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
argParserInfo = O.info (O.helper <*> argParser) mempty

-- Log an action (prefixes "-> ")
action :: String -> IO ()
action = putStrLn . ("-> " ++)

-- Could probably implement using EitherT, but too lazy :)
loadThemes :: B.Theme -> [FilePath] -> IO (Either String B.Theme)
loadThemes theme [] = return $ Right theme
loadThemes theme (path:paths) = do
  action $ "Loading theme " ++ show path ++ "."
  eModifiedTheme <- B.loadCustomizations path theme
  case eModifiedTheme of
    Left e  -> return $ Left e
    Right t -> loadThemes t paths

main :: IO ()
main = do
  options <- O.execParser argParserInfo

  -- Good ol' debug print
  if False then putStrLn "- The Options -" >> (putStrLn $ show options) else return ()

  -- Exporting default theme
  forM_ (oExportDefaultTheme options) $ \path -> do
    action $ "Exporting default theme to " ++ show path ++ "."
    B.saveTheme path TM.defaultTheme

  -- Loading themes and running the program
  eTheme <- loadThemes TM.defaultTheme $ oThemePaths options
  case eTheme of
    Left errMsg -> die errMsg
    Right theme -> error "Implement actual program logic" theme

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
