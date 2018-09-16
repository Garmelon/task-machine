module TaskMachine.UI.TopBar where

import qualified Brick                as B

placeholderTopBar :: B.Widget n
placeholderTopBar = B.str "Prune | Reload | Search: " B.<+> B.vLimit 1 (B.fill '_')
