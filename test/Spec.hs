import           Test.Hspec

import           TapeSpec          (tapeTests)
import           TuringMachineSpec (turingMachineTests)

main :: IO ()
main = hspec $ do
  tapeTests
  turingMachineTests
