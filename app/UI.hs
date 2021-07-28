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
import           TuringMachine              (From, State, To, runTransition)

------------------------------------------------
-- Types
------------------------------------------------

data Status = Status
  { machine       :: MachineCode
  , history       :: [Status]
  , currentState  :: State String
  , currentSymbol :: Symbol String
  , tape          :: Tape String
  , errorMessage  :: Maybe String
  , isFinal       :: Bool
  }
data Tick = Tick
type Name = ()

------------------------------------------------
-- Top level functions
------------------------------------------------

-- | Functions that runs the app with some defaults
runUiWith :: MachineCode -> IO Status
runUiWith code = defaultMain app $
  Status
    { machine = code
    , history = []
    , currentState = initialState code
    , currentSymbol = value (initialTape code)
    , tape = initialTape code
    , errorMessage = Nothing
    , isFinal = False
    }

-- | The app definition
app :: App Status Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const (attrMap V.defAttr [])
  }

------------------------------------------------
-- Event handling
------------------------------------------------

handleEvent :: Status -> BrickEvent Name Tick -> EventM Name (Next Status)
handleEvent m (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ executeStep m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue $ goBack m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt m
handleEvent m (VtyEvent (V.EvKey V.KEsc []))        = halt m
handleEvent m _                                     = continue m

executeStep :: Status -> Status
executeStep s = updateStatus s (transition s)

------------------------------------------------
-- Status update
------------------------------------------------

-- | Executes a single machine, setting the error if necessary and checking if
-- the machine has reached a final state
updateStatus :: Status -> Either (From String String) (To String String) -> Status
updateStatus status (Left _) = status{ errorMessage = Just "Invalid state reached" }
updateStatus status (Right (s', out, dir)) =
  let t = move dir $ write out $ tape status
      fs = finalStates (machine status)
      prevs = history status
  in status
    { currentState = s'
    , currentSymbol = value t
    , tape = t
    , isFinal = s' `elem` fs
    , history = status : prevs
    }

-- | Runs a single transition on @Status@
transition :: Status -> Either (From String String) (To String String)
transition (Status m _ state symbol _ _ _) = runTransition (transitions m) (state, symbol)

goBack :: Status -> Status
goBack status = goBack' $ history status
  where
    goBack' []    = status
    goBack' (h:_) = h

------------------------------------------------
-- Drawing
------------------------------------------------

-- | Draws the entire UI
drawUI :: Status -> [Widget Name]
drawUI status =
  [ drawTitle status
  <=> drawTape status
  <=> (drawCurrent status <+> drawNext status <+> drawPrevious status)
  <=> drawInstructions
  ]

-- | Draws the title, displaying the error message (if there's any) or if the
-- computation has terminated
drawTitle :: Status -> Widget n
drawTitle status = drawTitle' (errorMessage status) (isFinal status)
  where
    drawTitle' (Just msg) _ = str $ " !!! " ++ msg ++ " !!! "
    drawTitle' _ True       = str " === You reached the end! ==="
    drawTitle' _ _          = str "~ ~ ~ The Hasking Simulator ~ ~ ~"

-- | Draws the tape box
drawTape :: Status -> Widget Name
drawTape status =
  box totalWidth 8 "Tape" $ vBox
    [ padBottom (Pad 1) $ str tapeString
    , str (leftSpaces ++ "∆")
    ]
  where
    fixedList = toFixedList 15 (tape status)
    tapeString = unwords . map pretty $ fixedList
    halfTapeString = length . unwords . map pretty . take 15 $ fixedList
    leftSpaces = replicate (halfTapeString + 1) ' '
    totalWidth = length tapeString + 2

-- | Draws the current state box
drawCurrent :: Status -> Widget Name
drawCurrent status = statusBox "Current"
    ( pretty (currentState status)
    , pretty (currentSymbol status)
    , Nothing
    )

-- | Draws the next state box
drawNext :: Status -> Widget Name
drawNext status = statusBox "Next" (s, written, Just dir)
  where
    (s, written, dir) = case transition status of
      Right (s', out, d) -> (pretty s', pretty out, pretty d)
      Left _             -> ("-", "-", "-")

-- | Draws the previous state box
drawPrevious :: Status -> Widget Name
drawPrevious = statusBox "Previous" . drawPrevious' . history
  where
    drawPrevious' []    = ("-", "-", Nothing)
    drawPrevious' (h:_) = (pretty $ currentState h, pretty $ currentSymbol h, Nothing)

-- | Draws the instructions box
drawInstructions :: Widget Name
drawInstructions = box 63 9 "Instructions" $ vBox
  [ str "n - Go to the next state, i.e. run a single instruction"
  , str "b - Go back in history (stays the same if there's none)"
  , str "q - Quit the program"
  ]

-- | A generic box for a status
statusBox :: String -> (String, String, Maybe String) -> Widget Name
statusBox title (state, written, dir) =
  box 21 9 title $ vBox
    [ str ("State:  " ++ state)
    , str ("Symbol: " ++ written)
    , str lastString
    ]
  where
    lastString = case dir of
      Nothing -> " "
      Just d  -> "Dir:    " ++ d

-- | A generic box
box :: Int -> Int -> String -> Widget Name -> Widget Name
box width height title content =
  vLimit height $ hLimit width $ vBox
    [ withBorderStyle BS.unicodeRounded
    $ B.borderWithLabel (str title)
    $ C.hCenter
    $ padAll 1 content
    ]
