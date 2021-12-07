{-# LANGUAGE OverloadedStrings #-}

module UI (runUiWith) where

import           Brick                      hiding (Direction)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Maybe                 (fromMaybe)
import qualified Graphics.Vty               as V
import           Parser                     (Code (..))
import           Pretty                     (Pretty (pretty))
import           Tape                       (Symbol, Tape (..), toFixedList)
import           TuringMachine              hiding (machine)

------------------------------------------------
-- Types
------------------------------------------------

type TM = TuringMachine String String

data UIStatus = UIProcessing | UIFinished | UIError String
  deriving (Show, Eq)

data UIState = UIState
  { machine     :: TM
  , currentTape :: Tape String
  , uiPrevious  :: Maybe UIState
  , uiStatus    :: UIStatus
  , uiReload    :: IO Code
  }

type CustomEvent = ()
type Name = ()

------------------------------------------------
-- Interface
------------------------------------------------

-- | Functions that runs the app with some defaults
runUiWith :: TM -> Tape String -> IO Code -> IO UIState
runUiWith m t load = do
  defaultMain app $
    UIState
      { machine = m
      , currentTape = t
      , uiPrevious = Nothing
      , uiStatus = UIProcessing
      , uiReload = load
      }

-- | The app definition
app :: App UIState CustomEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const attributes
  }

------------------------------------------------
-- Event handling
------------------------------------------------

-- | Handles a generic event
handleEvent :: UIState -> BrickEvent Name CustomEvent -> EventM Name (Next UIState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue (executeStep s)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue (goBack s)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  Code m tapes <- liftIO (uiReload s)
  case tapes of
    []    -> continue s{ uiStatus = UIError "No tape found" }
    (t:_) -> continue $ s
      { machine = m
      , currentTape = t
      , uiPrevious = Nothing
      , uiStatus = UIProcessing
      }
handleEvent s _                                     = continue s

------------------------------------------------
-- Status update
------------------------------------------------

-- | Executes a single step forward if the machine hasn't UIfinished
executeStep :: UIState -> UIState
executeStep s@(UIState m t _ UIProcessing _) = updateUiState s (step m t)
executeStep s                                = s

-- | Updates the UI state based on the result of a single @step@
updateUiState :: UIState -> Either (From String String) (TM, Tape String) -> UIState
updateUiState state (Left _)       = state{ uiStatus = UIError "Invalid state reached" }
updateUiState state (Right (m, t)) =
  state
    { machine = m
    , currentTape = t
    , uiPrevious = Just state
    , uiStatus = if status m == Stopped then UIFinished else UIProcessing
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
  [ C.center $ drawTitle (uiStatus s)
  <=> drawTape s
  <=> (drawPrevious s <+> drawCurrent s <+> drawNext s)
  <=> drawInstructions
  ]

-- | Draws the title, displaying the UIerror message (if there's any) or if the
-- computation has terminated
drawTitle :: UIStatus -> Widget n
drawTitle = filled . drawTitle'
  where
    filled x =
      hLimit 63
      $ withBorderStyle BS.unicodeBold
      $ B.hBorder <+> str " " <+> x <+> str " " <+> B.hBorder

    drawTitle' (UIError msg) = withAttr errorAttr $ str msg
    drawTitle' UIFinished    = withAttr finishedAttr $ str "You reached the end!"
    drawTitle' UIProcessing  = withAttr titleAttr $ str "Hasking Simulator"

-- | Draws the tape box
drawTape :: UIState -> Widget Name
drawTape s =
  box 63 8 "Tape" $ vBox
    [ padBottom (Pad 1)
    $ normalTape leftStrings <+> str " " <+> currentVal cur <+> str " " <+> normalTape rightStrings
    , cursor "∆"
    ]
  where
    blinking = uiStatus s == UIProcessing
    normalTape = str . unwords
    currentVal = (if blinking then withAttr blinkAttr else id) . str
    cursor c = str (replicate halfTapeString  ' ') <+> str c

    fixedList = map pretty . toFixedList 15 $ currentTape s
    (leftStrings, cur:rightStrings) = splitAt 15 fixedList
    halfTapeString = length (unwords leftStrings) + 1

-- | Draws the current state box
drawCurrent :: UIState -> Widget Name
drawCurrent s = machineBox "Current"
    [ Just (pretty state)
    , Just (pretty val)
    , Nothing
    ]
  where (state, val) = currentFrom s

-- | Draws the next state box
drawNext :: UIState -> Widget Name
drawNext s = machineBox "Next" [Just s', Just written, Just dir]
  where
    m = machine s
    from = (current m, value $ currentTape s)
    (s', written, dir) = case transition from (transitions m) of
      Just (s', out, d) -> (pretty s', pretty out, pretty d)
      Nothing           -> ("-", "-", "-")

-- | Draws the previous state box
drawPrevious :: UIState -> Widget Name
drawPrevious = machineBox "Previous" . getInfo' . uiPrevious
  where
    getInfo' Nothing = [Just "-", Just "-", Nothing]
    getInfo' (Just p)  =
      let (s, v) = currentFrom p
      in [Just (pretty s), Just (pretty v), Nothing]

-- | Draws the instructions box
drawInstructions :: Widget Name
drawInstructions = box 63 9 "Instructions" $ vBox
  [ str "n - Go to the next state, i.e. run a single instruction"
  , str "b - Go back in history (stays the same if there's none)"
  , str "r - Reload the machine file"
  , str "q - Quit the program"
  ]

------------------------------------------------
-- Utilities
------------------------------------------------

currentFrom :: UIState -> (State String, Symbol String)
currentFrom s = (current $ machine s, value $ currentTape s)

-- | Draws a machine box displaying @State@, @Symbol@ and @Direction@
machineBox :: String -> [Maybe String] -> Widget Name
machineBox title = statusBox title . machineInfo
  where
    machineInfo = map str . zipWith formatStr ["State:  ", "Symbol: ", "Dir:    "]
    formatStr _ Nothing    = " "
    formatStr s1 (Just s2) = s1 ++ s2

-- | A generic box for a status
statusBox :: String -> [Widget Name] -> Widget Name
statusBox title content = box 21 9 title $ vBox content

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
  [ (errorAttr, fg V.red `V.withStyle` V.bold)
  , (finishedAttr, fg V.blue `V.withStyle` V.bold)
  , (titleAttr, fg V.white `V.withStyle` V.bold)
  , (blinkAttr, fg V.white `V.withStyle` V.blink)
  ]

errorAttr :: AttrName
errorAttr = "UIerrorAttr"

finishedAttr :: AttrName
finishedAttr = "UIfinishedAttr"

titleAttr :: AttrName
titleAttr = "titleAttr"

blinkAttr :: AttrName
blinkAttr = "blinkAttr"
