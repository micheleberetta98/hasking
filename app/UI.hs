{-# LANGUAGE OverloadedStrings #-}

module UI
  ( runUiWith
  , Status
  , Tick(Tick)
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
import           TuringMachine              (From, State, To, runTransition)

------------------------------------------------
-- Types
------------------------------------------------

data Status = Status
  { machine         :: MachineCode
  , history         :: [Status]
  , currentState    :: State String
  , currentSymbol   :: Symbol String
  , tape            :: Tape String
  , invalidStateMsg :: Maybe String
  , isFinal         :: Bool
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
    , invalidStateMsg = Nothing
    , isFinal = False
    }

-- | The app definition
app :: App Status Tick Name
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
handleEvent :: Status -> BrickEvent Name Tick -> EventM Name (Next Status)
handleEvent m (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ executeStep m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue $ goBack m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt m
handleEvent m (VtyEvent (V.EvKey V.KEsc []))        = halt m
handleEvent m _                                     = continue m

------------------------------------------------
-- Status update
------------------------------------------------

-- | Executes a single step forward if the machine hasn't finished
executeStep :: Status -> Status
executeStep s
  | isFinal s = s
  | otherwise = updateStatus s (transition s)


-- | Executes a single machine step, setting the error if necessary and checking if
-- the machine has reached a final state
updateStatus :: Status -> Either (From String String) (To String String) -> Status
updateStatus status (Left _) = status{ invalidStateMsg = Just "Invalid state reached" }
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
transition status = runTransition ts (state, symbol)
  where
    ts = transitions $ machine status
    state = currentState status
    symbol = currentSymbol status

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
  <=> drawTape (tape status)
  <=> (drawPrevious status <+> drawCurrent status <+> drawNext status)
  <=> drawInstructions
  ]

-- | Draws the title, displaying the error message (if there's any) or if the
-- computation has terminated
drawTitle :: Status -> Widget n
drawTitle status = drawTitle' (invalidStateMsg status) (isFinal status)
  where
    drawTitle' (Just msg) _ = withAttr errorAttr $ str (" !!! " ++ msg ++ " !!! ")
    drawTitle' _ True       = withAttr finishedAttr $ str " === You reached the end! ==="
    drawTitle' _ _          = str " ~~~ The Hasking Simulator ~~~"

-- | Draws the tape box
drawTape :: Tape String -> Widget Name
drawTape t =
  box 63 8 "Tape" $ vBox
    [ padBottom (Pad 1) $ str tapeString
    , str (leftSpaces ++ "∆")
    ]
  where
    fixedList = toFixedList 15 t
    tapeString = unwords . map pretty $ fixedList
    halfTapeString = length . unwords . map pretty . take 15 $ fixedList
    leftSpaces = replicate (halfTapeString + 1) ' '

-- | Draws the current state box
drawCurrent :: Status -> Widget Name
drawCurrent status = statusBox "Current" $ statusContent
    [ Just $ pretty (currentState status)
    , Just $ pretty (currentSymbol status)
    , Nothing
    ]

-- | Draws the next state box
drawNext :: Status -> Widget Name
drawNext status = statusBox "Next" . statusContent $ map Just [s, written, dir]
  where
    (s, written, dir) = case transition status of
      Right (s', out, d) -> (pretty s', pretty out, pretty d)
      Left _             -> ("-", "-", "-")

-- | Draws the previous state box
drawPrevious :: Status -> Widget Name
drawPrevious = statusBox "Previous" . statusContent . drawPrevious' . history
  where
    drawPrevious' []    = [Just "-", Just "-", Nothing]
    drawPrevious' (h:_) =
      [ Just . pretty $ currentState h
      , Just . pretty $ currentSymbol h
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
statusContent :: [Maybe String] -> [Widget Name]
statusContent = map str . zipWith formatStr ["State:  ", "Symbol: ", "Dir:    "]
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
  [ (errorAttr, fg V.red)
  , (finishedAttr, fg V.blue `V.withStyle` V.bold)
  ]

errorAttr :: AttrName
errorAttr = "errorAttr"

finishedAttr :: AttrName
finishedAttr = "finishedAttr"
