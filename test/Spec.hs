import           Test.Hspec

import           CodeSpec          (codeTests)
import           InstructionParserSpec   (instructionTests)
import           ParserSpec        (parserTests)
import           TapeSpec          (tapeTests)
import           TuringMachineSpec (turingMachineTests)

main :: IO ()
main = hspec $ do
  tapeTests
  turingMachineTests
  parserTests
  instructionTests
  codeTests
