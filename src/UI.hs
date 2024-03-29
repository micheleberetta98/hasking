module UI (runUiWith) where

import           Brick                      hiding (Direction)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Code                       (Code, Expression (..))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Maybe                 (fromMaybe)
import qualified Graphics.Vty               as V
import           Pretty                     (Pretty (pretty))
import           Tape                       (Direction, Symbol, Tape (..),
                                             toFixedList)
import           TuringMachine              hiding (machine)

------------------------------------------------
-- Types
------------------------------------------------

type TM = TuringMachine String

data UIStatus = Processing | Finished | Error String
  deriving (Show, Eq)

data UIState = UIState
  { machine     :: TM
  , currentTape :: Tape
  , uiPrevious  :: Maybe UIState
  , uiStatus    :: UIStatus
  }

type CustomEvent = ()
type Name = ()

data Visualized a = DontShow | Missing | Visualized a

------------------------------------------------
-- Instances
------------------------------------------------

instance Pretty a => Pretty (Visualized a) where
  pretty DontShow       = ""
  pretty Missing        = "-"
  pretty (Visualized a) = pretty a

------------------------------------------------
-- Interface
------------------------------------------------

-- | Functions that runs the app with some defaults
runUiWith :: TM -> Tape -> IO UIState
runUiWith m t = do
  defaultMain app $
    UIState
      { machine = m
      , currentTape = t
      , uiPrevious = Nothing
      , uiStatus = Processing
      }

-- | The app definition
app :: App UIState CustomEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const attributes
  }

------------------------------------------------
-- Event handling
------------------------------------------------

-- | Handles a generic event
handleEvent :: BrickEvent Name CustomEvent -> EventM Name UIState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = modify executeStep
handleEvent (VtyEvent (V.EvKey (V.KChar 'b') [])) = modify goBack
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent _                                     = continueWithoutRedraw

------------------------------------------------
-- Status update
------------------------------------------------

-- | Executes a single step forward if the machine hasn't UIfinished
executeStep :: UIState -> UIState
executeStep s@(UIState m t _ Processing) = updateUiState s (step m t)
executeStep s                            = s

-- | Updates the UI state based on the result of a single @step@
updateUiState :: UIState -> Either (From String) (TM, Tape) -> UIState
updateUiState state (Left _)       = state{ uiStatus = Error "Invalid state reached" }
updateUiState state (Right (m, t)) =
  state
    { machine = m
    , currentTape = t
    , uiPrevious = Just state
    , uiStatus = if status m == Stopped then Finished else Processing
    }

-- | Go back in history (if there's any)
goBack :: UIState -> UIState
goBack = fromMaybe <$> id <*> uiPrevious

------------------------------------------------
-- Main parts
------------------------------------------------

-- | Draws the entire UI
drawUI :: UIState -> [Widget Name]
drawUI s =
  [   C.center $ drawTitle (uiStatus s)
  <=> drawTape s
  <=> (drawPrevious s <+> drawCurrent s <+> drawNext s)
  <=> drawInstructions
  ]

-- | Draws the title, displaying the UIerror message (if there's any) or if the
-- computation has terminated
drawTitle :: UIStatus -> Widget n
drawTitle = hLimit 63 . withBorderStyle BS.unicodeBold . lateralBorders . drawTitle'
  where
    lateralBorders x = B.hBorder <+> str " " <+> x <+> str " " <+> B.hBorder
    drawTitle' (Error msg) = withAttr errorAttr    $ str msg
    drawTitle' Finished    = withAttr finishedAttr $ str "You reached the end!"
    drawTitle' Processing  = withAttr titleAttr    $ str "Hasking Simulator"

-- | Draws the tape box
drawTape :: UIState -> Widget Name
drawTape s =
  box 63 8 "Tape" $ vBox
    [ padBottom (Pad 1)
    $ normalTape leftStrings <+> str " " <+> currentVal cur <+> str " " <+> normalTape rightStrings
    , cursor "∆"
    ]
  where
    normalTape = str . unwords
    currentVal = (if uiStatus s == Processing then withAttr blinkAttr else id) . str
    cursor c = str (replicate halfTapeString  ' ') <+> str c

    fixedList = map pretty . toFixedList 15 $ currentTape s
    (leftStrings, cur:rightStrings) = splitAt 15 fixedList
    halfTapeString = length (unwords leftStrings) + 1

-- | Draws the current state box
drawCurrent :: UIState -> Widget Name
drawCurrent s = machineBox "Current" (Visualized state, Visualized val, DontShow)
  where (state, val) = currentFrom s

-- | Draws the next state box
drawNext :: UIState -> Widget Name
drawNext s = machineBox "Next" $
  case transition from (transitions m) of
    Just (s', out, d) -> (Visualized s', Visualized out, Visualized d)
    Nothing           -> (Missing, Missing, DontShow)
  where
    m = machine s
    from = (current m, value $ currentTape s)

-- | Draws the previous state box
drawPrevious :: UIState -> Widget Name
drawPrevious = machineBox "Previous" . getInfo' . uiPrevious
  where
    getInfo' Nothing  = (Missing, Missing, DontShow)
    getInfo' (Just p) = let (s, v) = currentFrom p in (Visualized s, Visualized v, DontShow)

-- | Draws the instructions box
drawInstructions :: Widget Name
drawInstructions = box 63 9 "Instructions" $ vBox
  [ str "n - Go to the next state, i.e. run a single instruction"
  , str "b - Go back in history (stays the same if there's none)"
  , str "q - Quit the program"
  ]

------------------------------------------------
-- Utilities
------------------------------------------------

currentFrom :: UIState -> (State String, Symbol)
currentFrom s = (current $ machine s, value $ currentTape s)

-- | Draws a machine box displaying @State@, @Symbol@ and @Direction@
machineBox :: String -> (Visualized (State String), Visualized Symbol, Visualized Direction ) -> Widget Name
machineBox title (state, symbol, dir) =  box 21 9 title $ vBox
  [ str $ prepend "State:  " state
  , str $ prepend "Symbol: " symbol
  , str $ prepend "Dir:    " dir
  ]
  where
    prepend _ DontShow = " "
    prepend s1 s2      = s1 ++ pretty s2

-- | A generic box
box :: Int -> Int -> String -> Widget Name -> Widget Name
box width height title content =
  vLimit height $ hLimit width $ vBox
    [ withBorderStyle BS.unicodeRounded
    $ B.borderWithLabel (str title)
    $ C.hCenter
    $ padAll 1 content
    ]

------------------------------------------------
-- Attributes
------------------------------------------------

-- | The attributes map, that defines styles
attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (errorAttr,    fg V.red   `V.withStyle` V.bold)
  , (finishedAttr, fg V.green `V.withStyle` V.bold)
  , (titleAttr,    fg V.white `V.withStyle` V.bold)
  , (blinkAttr,    fg V.white `V.withStyle` V.blink)
  ]

errorAttr :: AttrName
errorAttr = attrName "UIerrorAttr"

finishedAttr :: AttrName
finishedAttr = attrName "UIfinishedAttr"

titleAttr :: AttrName
titleAttr = attrName "titleAttr"

blinkAttr :: AttrName
blinkAttr = attrName "blinkAttr"
