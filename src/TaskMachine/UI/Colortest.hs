{-# LANGUAGE OverloadedStrings #-}

-- | A collection of types necessary for the UI.
--
-- These were put in a separate module to avoid an import cycle.

module TaskMachine.UI.Colortest where

import           Control.Monad
import           Data.List

import qualified Brick         as B
import qualified Graphics.Vty  as VTY

colors :: [(String, VTY.Color)]
colors =
  [ ("black",   VTY.black)
  , ("red",     VTY.red)
  , ("green",   VTY.green)
  , ("yellow",  VTY.yellow)
  , ("blue",    VTY.blue)
  , ("magenta", VTY.magenta)
  , ("cyan",    VTY.cyan)
  , ("white",   VTY.white)
  , ("brightBlack",   VTY.brightBlack)
  , ("brightRed",     VTY.brightRed)
  , ("brightGreen",   VTY.brightGreen)
  , ("brightYellow",  VTY.brightYellow)
  , ("brightBlue",    VTY.brightBlue)
  , ("brightMagenta", VTY.brightMagenta)
  , ("brightCyan",    VTY.brightCyan)
  , ("brightWhite",   VTY.brightWhite)
  ]

styles :: [(String, VTY.Style)]
styles =
  [ ("standout",     VTY.standout)
  , ("underline",    VTY.underline)
--  , ("reverseVideo", VTY.reverseVideo)
--  , ("blink",        VTY.blink)
--  , ("dim",          VTY.dim)
  , ("bold",         VTY.bold)
  ]

toName :: String -> String -> String -> B.AttrName
toName a b c = B.attrName a <> B.attrName b <> B.attrName c

useStyles :: [VTY.Style] -> VTY.Attr -> VTY.Attr
useStyles = foldr ((.) . flip VTY.withStyle) id

attrMap :: B.AttrMap
attrMap = B.attrMap VTY.defAttr $ do
  (fgName, fgColor) <- colors
  (bgName, bgColor) <- colors
  styleList <- subsequences styles
  let styleName = concatMap fst styleList
      name   = toName styleName bgName fgName
      fgAttr = VTY.withForeColor VTY.defAttr fgColor
      bgAttr = VTY.withBackColor fgAttr      bgColor
      attr   = useStyles (map snd styleList) bgAttr
  pure (name, attr)

cw :: String -> B.Widget n
cw style = B.vBox $ B.str (' ':style) : do
  (bgName, _) <- colors
  pure $ B.hBox $ do
    (fgName, _) <- colors
    let name = toName style bgName fgName
    pure $ B.withAttr name $ B.str "Hi"

testWidget :: B.Widget n
testWidget = B.vBox
  [ B.hBox [cw "", cw "standout"]
  , B.hBox [cw "", cw "underline"]
--  , B.hBox [cw "", cw "reverseVideo"]
--  , B.hBox [cw "", cw "blink"]
--  , B.hBox [cw "", cw "dim"]
  , B.hBox [cw "", cw "bold"]
  ]

--fgAttrs :: [(B.AttrName, VTY.Attr)]
--fgAttrs = map toFGAttr colors
--  where
--    toFGAttr :: (String, VTY.Color) -> (B.AttrName, VTY.Attr)
--    toFGAttr (s, c) = (toFGName s, VTY.withForeColor VTY.currentAttr c)
--
--bgAttrs :: [(B.AttrName, VTY.Attr)]
--bgAttrs = map toBGAttr colors
--  where
--    toBGAttr :: (String, VTY.Color) -> (B.AttrName, VTY.Attr)
--    toBGAttr (s, c) = (toBGName s, VTY.withBackColor VTY.currentAttr c)
--
--styleAttrs :: [(B.AttrName, VTY.Attr)]
--styleAttrs = map toStyleAttr styles
--  where
--    toStyleAttr :: (String, VTY.Style) -> (B.AttrName, VTY.Attr)
--    toStyleAttr (s, st) = (toStyleName s, VTY.withStyle VTY.currentAttr st)
--
--attrMap :: B.AttrMap
--attrMap = B.attrMap VTY.defAttr $ concat [fgAttrs, bgAttrs, styleAttrs]
--
--colorWidget :: B.Widget n
--colorWidget = B.vBox $ do
--  (bgName, _) <- colors
--  let name = toBGName bgName
--  pure $ B.withAttr name $ B.hBox $ do
--    (fgName, _) <- colors
--    let name = toFGName fgName
--    pure $ B.withAttr name $ B.str "Hi"
--
--testWidget :: B.Widget n
--testWidget = B.vBox $ do
--  (styleName, _) <- styles
--  let label = B.str styleName
--      name = toStyleName styleName
--      widget = B.withAttr name colorWidget
--  pure $ B.vBox [B.str "", label, widget]
----  sStyles <- subsequences styles
----  let label = B.str . concat . intercalate ", " . map fst $ sStyles
----      styleMod = foldr (.) id  $ map (flip VTY.withStyle . snd) sStyles
----      attr = styleMod VTY.defAttr
----      widget = B.withAttr colorWidget
----  pure $ B.vBox [B.str "", label, widget]

colorTestMain :: IO ()
colorTestMain = void $ B.defaultMain app ()
  where
    app :: B.App () () ()
    app = B.App
      { B.appDraw = const [testWidget]
      , B.appChooseCursor = B.neverShowCursor
      , B.appHandleEvent = B.resizeOrQuit
      , B.appStartEvent = pure
      , B.appAttrMap = const attrMap
      }
