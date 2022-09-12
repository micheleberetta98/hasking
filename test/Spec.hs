import           Test.Hspec

import           ParserSpec
import           TapeSpec
import           TuringMachineSpec

main :: IO ()
main = hspec $ do
  tapeTests
  turingMachineTests
  parserTests
