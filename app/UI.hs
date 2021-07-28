module UI
  ( runUiWith
  , Status
  , Tick(Tick)
  ) where

import           Brick                      hiding (Direction)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
-- import           Data.Sequence              (Seq)
-- import qualified Data.Sequence              as S
import           Code
import qualified Graphics.Vty               as V
import           Pretty                     (Pretty (pretty))
import           Tape
import           TuringMachine              (State, runTransition)

------------------------------------------------
-- Types
------------------------------------------------

data Status = Status
  { machine       :: MachineCode
  , history       :: [Status]
  , currentState  :: State String
  , currentSymbol :: Symbol String
  , tape          :: Tape String
  }
data Tick = Tick
type Name = ()

------------------------------------------------
-- Functions
------------------------------------------------

runUiWith :: MachineCode -> IO Status
runUiWith code = defaultMain app $
  Status
    { machine = code
    , history = []
    , currentState = initialState code
    , currentSymbol = value (initialTape code)
    , tape = initialTape code
    }

app :: App Status Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const (attrMap V.defAttr [])
  }

handleEvent :: Status -> BrickEvent Name Tick -> EventM Name (Next Status)
handleEvent m (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue $ m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt m
handleEvent m (VtyEvent (V.EvKey V.KEsc []))        = halt m
handleEvent m _                                     = continue m

drawUI :: Status -> [Widget Name]
drawUI m =
  [ str "The Hasking Simulator!"
  <=> (drawTape m <+> drawInstructions)
  <=> (drawCurrent m <+> drawNext m <+> drawPrevious m)
  ]

drawTape :: Status -> Widget Name
drawTape (Status _ _ _ _ t) =
  box totalWidth 8 "Tape" $ vBox
    [ padBottom (Pad 1) $ str tapeString
    , str (leftSpaces ++ "^")
    ]
  where
    fixedList = toFixedList 10 t
    tapeString = unwords . map pretty $ fixedList
    halfTapeString = length . unwords . map pretty . take 10 $ fixedList
    leftSpaces = replicate (halfTapeString + 1) ' '
    totalWidth = length tapeString + 2

drawCurrent :: Status -> Widget Name
drawCurrent (Status _ _ state symbol _) =
    statusBox "Current" (pretty state, pretty symbol, Nothing)

drawNext :: Status -> Widget Name
drawNext (Status m _ state symbol _) = statusBox "Next" (s, written, Just dir)
  where
    (s, written, dir) =
      case runTransition (transitions m) (state, symbol) of
        Right (s', out, d) -> (pretty s', pretty out, pretty d)
        Left _             -> ("-", "-", "-")

drawPrevious :: Status -> Widget Name
drawPrevious = statusBox "Previous" . drawPrevious'
  where
    drawPrevious' (Status _ [] _ _ _)    = ("-", "-", Nothing)
    drawPrevious' (Status _ (h:_) _ _ _) =
      (pretty $ currentState h, pretty $ currentSymbol h, Nothing)


drawInstructions :: Widget Name
drawInstructions = box 20 9 "Instructions" $ vBox
  [ str "n - Go to next state"
  , str "b - Go back in history"
  , str "q - Quit"
  ]

statusBox :: String -> (String, String, Maybe String) -> Widget Name
statusBox title (state, written, dir) =
  box 21 9 title $ vBox
    [ str ("State:   " ++ state)
    , str ("Written: " ++ written)
    , str lastString
    ]
  where
    lastString = case dir of
      Nothing -> " "
      Just d  -> "Dir:     " ++ d

box :: Int -> Int -> String -> Widget Name -> Widget Name
box width height title content =
  vLimit height $ hLimit width $ vBox
    [ withBorderStyle BS.unicodeRounded
    $ B.borderWithLabel (str title)
    $ C.hCenter
    $ padAll 1 content
    ]
