module Tableaux where

import Proposition
import           Data.List
import qualified Data.Set as Set
import           Data.Set (Set)
    
data Valuation = Valuation Char Bool deriving (Eq, Ord)

instance Show Valuation where
  show (Valuation atom True)  = show atom ++ "==" ++ "t"
  show (Valuation atom False) = show atom ++ "==" ++ "f"

isValid :: Valuation -> Valuation -> Bool
isValid (Valuation atom1 val1) (Valuation atom2 val2) = atom1 /= atom2 || val1 == val2

isValidBranch :: Set Valuation -> Bool
isValidBranch set = validator (Set.toList set) 
  where
    validator []     = False 
    validator [_]    = True
    validator (x:xs) =
      all (isValid x) xs && validator xs

expand :: Proposition -> Set(Set Valuation)
expand (Atomic a)                      = Set.singleton(Set.singleton (Valuation a True))
expand (Negation (Atomic a))           = Set.singleton(Set.singleton (Valuation a False))
expand (Negation (Negation e))         = expand e                                            -- Double Negation
expand (Negation (Conjunction e1 e2))  = expand (Disjunction  (Negation e1) (Negation e2) )   -- De Morgan's laws
expand (Negation (Disjunction  e1 e2)) = expand (Conjunction (Negation e1) (Negation e2))    -- De Morgan's laws
expand (Implication e1 e2)             = expand (Disjunction (Negation e1) e2)
expand (Disjunction  e1 e2)            = Set.union (expand e1) (expand e2)
expand (Conjunction f1 f2)             = let
  s1 = expand f1
  s2 = expand f2
  s3 = Set.map (\g -> Set.map (\h ->Set.union g h) s2 ) s1
    in 
      Set.foldr (Set.union) Set.empty s3

proof :: Proposition -> Bool
proof prop = not $ Set.foldr (||) False  (Set.map (\n -> isValidBranch n) (expand (Negation prop)))