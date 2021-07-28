module UI
  ( runUiWith
  , Status
  , Tick(Tick)
  )
where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
-- import           Data.Sequence              (Seq)
-- import qualified Data.Sequence              as S
import           Code                       (MachineCode (initialState))
import qualified Graphics.Vty               as V
-- import           Tape
import           TuringMachine              (State)

------------------------------------------------
-- Types
------------------------------------------------

data Status = Status
  { machine      :: MachineCode
  , history      :: [Status]
  , currentState :: State String
  }
data Tick = Tick
type Name = ()

------------------------------------------------
-- Functions
------------------------------------------------

runUiWith :: MachineCode -> IO Status
runUiWith code = defaultMain appUi $
  Status{ machine = code, history = [], currentState = initialState code }

appUi :: App Status Tick Name
appUi = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

handleEvent :: Status -> BrickEvent Name Tick -> EventM Name (Next Status)
handleEvent m (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue $ m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt m
handleEvent m (VtyEvent (V.EvKey V.KEsc []))        = halt m
handleEvent m _                                     = continue m

drawUI :: Status -> [Widget Name]
drawUI m =
  [padRight (Pad 2) (drawStats m)]

drawStats :: p -> Widget Name
drawStats _ = hLimit 11
  $ vBox [ withBorderStyle BS.unicodeBold
          $ B.borderWithLabel (str "Hello")
          $ C.hCenter
          $ padAll 1
          $ str "Hello World!"]

theMap :: AttrMap
theMap = attrMap V.defAttr []
