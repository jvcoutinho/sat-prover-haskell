import Test.QuickCheck
import Resolution
import TruthTable
import Proposition

prop_ResolutionMethodEqualsTruthTableMethod :: Proposition -> Bool
prop_ResolutionMethodEqualsTruthTableMethod prop = Resolution.proof prop == TruthTable.proof prop