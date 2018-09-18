module TaskMachine.UI.Popup
  ( minPopupWidth
  -- * Ok popup
  , PopupOk
  , popupOk
  , popupOk'
  , renderPopupOk
  , handlePopupOkEvent
  ) where

import qualified Brick                as B
import qualified Brick.Widgets.Dialog as B
import qualified Graphics.Vty         as VTY

minPopupWidth :: Int
minPopupWidth = 78

{- Ok popup -}

data PopupOk n = PopupOk (B.Dialog ()) (B.Widget n)

popupOk :: String -> String -> PopupOk n
popupOk title content = popupOk' title (B.str content)

popupOk' :: String -> B.Widget n -> PopupOk n
popupOk' title widget =
  let dialog = B.dialog (Just $ " " ++ title ++ " ") (Just (0,[("Ok",())])) minPopupWidth
  in  PopupOk dialog widget

renderPopupOk :: PopupOk n -> B.Widget n
renderPopupOk (PopupOk dialog widget) = B.renderDialog dialog widget

handlePopupOkEvent :: VTY.Event -> PopupOk n -> B.EventM n (PopupOk n)
handlePopupOkEvent e (PopupOk dialog widget) = PopupOk <$> B.handleDialogEvent e dialog <*> pure widget
