import Data.List
import Data.Ord
import qualified Line
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps]

qcProps =
  testGroup
    "(checked by QuickCheck)"
    Line.tests
