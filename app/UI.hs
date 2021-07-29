{-# LANGUAGE OverloadedStrings #-}

module UI
  ( runUiWith
  , Status
  ) where

import           Brick                      hiding (Direction)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Code                       (MachineCode (finalStates, initialState, initialTape, transitions))
import qualified Graphics.Vty               as V
import           Pretty                     (Pretty (pretty))
import           Tape                       (Symbol, Tape (..), move,
                                             toFixedList, write)
import           TuringMachine              (From, State, To, Transitions,
                                             runTransition)

------------------------------------------------
-- Types
------------------------------------------------

data ProcessingState = Processing | Finished | Error String
  deriving (Show, Eq)

data MachineState = MachineState
  { trans  :: Transitions String String
  , state  :: State String
  , tape   :: Tape String
  , finals :: [State String]
  }

-- | A little helper
symbol :: MachineState -> Symbol String
symbol = value . tape

data Status = Status
  { machine         :: MachineState
  , history         :: [Status]
  , processingState :: ProcessingState
  }
type CustomEvent = ()
type Name = ()

------------------------------------------------
-- Top level functions
------------------------------------------------

-- | Functions that runs the app with some defaults
runUiWith :: MachineCode -> IO Status
runUiWith code = defaultMain app $
  Status
    { machine = MachineState
      { trans = transitions code
      , state = initialState code
      , tape = initialTape code
      , finals = finalStates code
      }
    , history = []
    , processingState = Processing
    }

-- | The app definition
app :: App Status CustomEvent Name
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
handleEvent :: Status -> BrickEvent Name CustomEvent -> EventM Name (Next Status)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ executeStep s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue $ goBack s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
handleEvent s _                                     = continue s

------------------------------------------------
-- Status update
------------------------------------------------

-- | Executes a single step forward if the machine hasn't finished
executeStep :: Status -> Status
executeStep s
  | processingState s == Finished = s
  | otherwise                     = updateStatus s (step $ machine s)


-- | Executes a single machine step, setting the error if necessary and checking if
-- the machine has reached a final state
updateStatus :: Status -> Either (From String String) (To String String) -> Status
updateStatus status (Left _) = status{ processingState = Error "Invalid state reached" }
updateStatus status (Right (state', out, dir)) =
  status
    { machine = m{ state = state', tape = tape' }
    , processingState = if isFinal state' then Finished else Processing
    , history = status : history status
    }
  where
    m = machine status
    tape' = move dir . write out . tape $ m
    isFinal = (`elem` finals m)

-- | Runs a single transition on @MachineState@
step :: MachineState -> Either (From String String) (To String String)
step m = runTransition (trans m) (state m, symbol m)

-- | Go back in history (if there's any)
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
  [ C.center $ drawTitle (processingState status)
  <=> drawTape m
  <=> (drawPrevious status <+> drawCurrent m <+> drawNext m)
  <=> drawInstructions
  ]
  where m = machine status

-- | Draws the title, displaying the error message (if there's any) or if the
-- computation has terminated
drawTitle :: ProcessingState -> Widget n
drawTitle = filled . drawTitle'
  where
    filled x = hLimit 63 $ B.hBorder <+> x <+> B.hBorder
    drawTitle' (Error msg) = withAttr errorAttr $ str msg
    drawTitle' Finished    = withAttr finishedAttr $ str "You reached the end!"
    drawTitle' Processing  = withAttr titleAttr $ str "Hasking Simulator"

-- | Draws the tape box
drawTape :: MachineState -> Widget Name
drawTape m =
  box 63 8 "Tape" $ vBox
    [ padBottom (Pad 1) $ str tapeString
    ,  str leftSpaces <+> str "∆"
    ]
  where
    fixedList = toFixedList 15 $ tape m
    tapeString = unwords . map pretty $ fixedList
    halfTapeString = length . unwords . map pretty . take 15 $ fixedList
    leftSpaces = replicate (halfTapeString + 1) ' '

-- | Draws the current state box
drawCurrent :: MachineState -> Widget Name
drawCurrent m = statusBox "Current" $ machineState
    [ Just $ pretty (state m)
    , Just $ pretty (symbol m)
    , Nothing
    ]

-- | Draws the next state box
drawNext :: MachineState -> Widget Name
drawNext m = statusBox "Next" . machineState $ map Just [s, written, dir]
  where
    (s, written, dir) = case step m of
      Right (s', out, d) -> (pretty s', pretty out, pretty d)
      Left _             -> ("-", "-", "-")

-- | Draws the previous state box
drawPrevious :: Status -> Widget Name
drawPrevious = statusBox "Previous" . machineState . drawPrevious' . history
  where
    drawPrevious' []    = [Just "-", Just "-", Nothing]
    drawPrevious' (h:_) =
      [ Just . pretty . state . machine $ h
      , Just . pretty . symbol . machine $ h
      , Nothing
      ]

-- | Draws the instructions box
drawInstructions :: Widget Name
drawInstructions = box 63 9 "Instructions" $ vBox
  [ str "n - Go to the next state, i.e. run a single instruction"
  , str "b - Go back in history (stays the same if there's none)"
  , str "q - Quit the program"
  ]

-- | A generic box for a status
statusBox :: String -> [Widget Name] -> Widget Name
statusBox title content = box 21 9 title $ vBox content

-- | It generates a list of @Widget Name@ that can be put into a @statusBox@,
-- showing @"State"@, @"Symbol"@ and @"Direction"@
machineState :: [Maybe String] -> [Widget Name]
machineState = map str . zipWith formatStr ["State:  ", "Symbol: ", "Dir:    "]
  where
    formatStr _ Nothing    = " "
    formatStr s1 (Just s2) = s1 ++ s2

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
  ]

errorAttr :: AttrName
errorAttr = "errorAttr"

finishedAttr :: AttrName
finishedAttr = "finishedAttr"

titleAttr :: AttrName
titleAttr = "titleAttr"
