module UI
  ( runUiWith
  , Machine(Machine)
  , Tick(Tick)
  )
where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
-- import           Data.Sequence              (Seq)
-- import qualified Data.Sequence              as S
import qualified Graphics.Vty               as V

------------------------------------------------
-- Types
------------------------------------------------

data Machine = Machine
data Tick = Tick
type Name = ()

------------------------------------------------
-- Functions
------------------------------------------------

runUiWith :: Machine -> IO Machine
runUiWith = defaultMain appUi

appUi :: App Machine Tick Name
appUi = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

handleEvent :: Machine -> BrickEvent Name Tick -> EventM Name (Next Machine)
handleEvent m (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue $ m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt m
handleEvent m (VtyEvent (V.EvKey V.KEsc []))        = halt m
handleEvent m _                                     = continue m

drawUI :: Machine -> [Widget Name]
drawUI m =
  [C.center $ padRight (Pad 2) (drawStats m)]

drawStats :: p -> Widget Name
drawStats _ = hLimit 11
  $ vBox [ withBorderStyle BS.unicodeBold
          $ B.borderWithLabel (str "Hello")
          $ C.hCenter
          $ padAll 1
          $ str "Hello World!"]

theMap :: AttrMap
theMap = attrMap V.defAttr []
