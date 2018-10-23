module TaskMachine.UI.Popup
  ( Popup
  , popup
  , popup'
  , renderPopup
  , handlePopupEvent
  , popupSelection
  , minPopupWidth
  ) where

import qualified Brick                as B
import qualified Brick.Widgets.Dialog as B
import qualified Graphics.Vty         as VTY

data Popup n r = Popup (B.Dialog r) (B.Widget n)

popup :: String -> String -> [(String, r)] -> Popup n r
popup title content = popup' title (B.str content)

popup' :: String -> B.Widget n -> [(String, r)] -> Popup n r
popup' title widget results =
  let spacedTitle = " " ++ title ++ " "
      dialog = B.dialog (Just spacedTitle) (Just (0, results)) minPopupWidth
  in  Popup dialog widget

renderPopup :: Popup n r -> B.Widget n
renderPopup (Popup dialog widget) = B.renderDialog dialog widget

handlePopupEvent :: VTY.Event -> Popup n r -> B.EventM n (Popup n r)
handlePopupEvent e (Popup dialog widget) = Popup <$> B.handleDialogEvent e dialog <*> pure widget

popupSelection :: Popup n r -> Maybe r
popupSelection (Popup dialog _) = B.dialogSelection dialog

minPopupWidth :: Int
minPopupWidth = 78
