module Code
  ( MachineCode(..)
  , parseCode
  ) where

import           Data.List     (foldl')
import qualified Data.Map      as M
import           Instruction   (Instruction (Control, Step, TapeValue),
                                parseInstruction)
import           LineError     (ErrorList, LineError, justOne, linedError,
                                simpleError, (.+), (.++))
import           Tape          (Direction, Symbol, Tape)
import qualified Tape          as T
import           TuringMachine (FromState, State (State), StateList, ToState,
                                Transitions)

------------------------------------------------
-- Data types
------------------------------------------------

-- | The raw lines that define the behaviour of the machine, each line defines an `Instruction`
type Code = String

-- | A line with its number
type LineCode = (Int, String)

-- | An instruction with its line number
type LineInstruction = (Int, Instruction)

type From = FromState String String
type To = ToState String String
type WithError = Either ErrorList

-- | The whole machine specification, derived from a piece of code
data MachineCode = MachineCode
  { transitions  :: Transitions String String
  , initialState :: State String
  , finalStates  :: StateList String
  , initialTape  :: Tape String
  } deriving (Show, Eq)

------------------------------------------------
-- Functions
------------------------------------------------

-- | An empty `MachineCode` structure
empty :: MachineCode
empty = MachineCode M.empty (State "") [] T.empty

-- | It converts the code into a `MachineCode` structure, with transitions, initial and final states
-- It returns `Nothing` if something goes wrong
parseCode :: Code -> WithError MachineCode
parseCode code = (buildCode . parseInstructions . stripComments $ lines' code) >>= mvalidate
  where
    buildCode = foldl' (<+>) (Right empty)
    parseInstructions = map (format . fmap parseInstruction)

    format :: (Int, Either String Instruction) -> Either LineError LineInstruction
    format (l, Left msg) = Left $ linedError msg l
    format (l, Right i)  = Right (l, i)

-- | Add line numbers to the code
lines' :: Code -> [LineCode]
lines' = zip [1..] . lines

-- | Removes a comment from a line
stripComments :: [LineCode] -> [LineCode]
stripComments = filter notEmpty . map (fmap (takeWhile (/= '#')))
  where notEmpty (_, l) = l /= ""

-- | Updates the machine code given a single instruction
updateCode :: MachineCode -> Instruction -> MachineCode
updateCode c (Control "BEGIN" [s]) = c{ initialState = s }
updateCode c (Control "FINAL" s)   = c{ finalStates = s }
updateCode c (TapeValue tape)      = c{ initialTape = T.fromList tape }
updateCode c s                     = addTransition (split s) c

-- | Inserts a new transitions into the existing ones
addTransition :: (From, To) -> MachineCode -> MachineCode
addTransition (from, to) c = c{ transitions = M.insert from to (transitions c) }

-- | It converts a `Step` instruction into a tuple made of `FromState` and `ToState`
split :: Instruction -> (From, To)
split (Step s1 v1 s2 v2 d) = (from, to)
  where
    from = (s1, v1)
    to = (s2, v2, d)

------------------------------------------------
-- Utils to combine errors
------------------------------------------------

-- | Combines an `WithError MachineCode` with a `Either LineError LineInstruction`
(<+>) :: WithError MachineCode -> Either LineError LineInstruction -> WithError MachineCode
Left es <+> Left e  = Left (es .+ e)
Left es <+> Right i = Left es <++> ivalidate i
Right c <+> Left e  = just e
Right c <+> Right i = updateCode c <$> ivalidate i

-- | Combines two `WithError ` together
(<++>) :: WithError a -> WithError b -> WithError a
Left es1 <++> Left es2 = Left (es1 .++ es2)
Right _ <++> Left es   = Left es
Left es <++> Right _   = Left es
Right x <++> Right _   = Right x

------------------------------------------------
-- validations
------------------------------------------------

-- | Validates the final machine code
mvalidate :: MachineCode -> WithError MachineCode
mvalidate m@(MachineCode tr s ss t) =
  (trans tr <++> initial s <++> finals ss <++> tape t) >> Right m
  where
    trans t
      | t == M.empty = just $ simpleError "No instructions provided"
      | otherwise    = Right ""
    initial (State "") = just $ simpleError "No initial state provided"
    initial _          = Right ""
    finals [] = just $ simpleError "No final states provided"
    finals _  = Right ""
    tape t
      | null (T.toList t) = just $ simpleError "No tape provided"
      | otherwise         = Right ""

-- | Validates the instruction
ivalidate :: LineInstruction -> WithError Instruction
ivalidate (l, i) = ivalidate' i l
  where
    ivalidate' (Control "BEGIN" [])  = just . linedError "No initial state provided"
    ivalidate' (Control "BEGIN" states)
      | length states > 1           = just . linedError "More than one inital state provided"
    ivalidate' (Control "FINALS" []) = just . linedError "No final states provided"
    ivalidate' (TapeValue [])        = just . linedError "Empty input tape provided"
    ivalidate' i                     = const (Right i)

-- | Utility that creates a single `Left (ErrorList [LineError])`
just :: LineError -> WithError b
just msg = Left $ justOne msg
