module Proposition where

import           Test.QuickCheck
import           Control.Monad

data Proposition = Atomic Char
                 | Negation Proposition
                 | Conjunction Proposition Proposition
                 | Disjunction Proposition Proposition
                 | Implication Proposition Proposition deriving (Eq)

instance Show Proposition where
  show (Atomic   c   ) = [c]
  show (Negation prop) = "(~" ++ show prop ++ ")"
  show (Conjunction prop1 prop2) =
    "(" ++ show prop1 ++ " . " ++ show prop2 ++ ")"
  show (Disjunction prop1 prop2) =
    "(" ++ show prop1 ++ " + " ++ show prop2 ++ ")"
  show (Implication prop1 prop2) =
    "(" ++ show prop1 ++ " > " ++ show prop2 ++ ")"

instance Arbitrary Proposition where
  arbitrary = sized proposition
   where
    proposition 0 = fmap Atomic arbitrary
    proposition n = oneof
      [ fmap Atomic   arbitrary
      , fmap Negation subexpression
      , liftM2 Conjunction subexpression subexpression
      , liftM2 Disjunction subexpression subexpression
      , liftM2 Implication subexpression subexpression
      ]
      where subexpression = proposition (n `div` 10)
