module Resolution where

import           Proposition
import           Data.List

proof :: Proposition -> Bool
proof prop = emptyClauseIsReachable . toCNF $ Negation prop

toCNF :: Proposition -> Proposition
-- Literals
toCNF (Atomic   c                                 ) = (Atomic c)
toCNF (Negation (Atomic c)                        ) = (Negation (Atomic c))
-- Conjunction
toCNF (Conjunction prop1 prop2) = Conjunction (toCNF prop1) (toCNF prop2)
-- Disjunction: distributive property
toCNF (Disjunction (Conjunction prop1 prop2) prop3) = toCNF $ Conjunction
  (toCNF $ Disjunction prop1 prop3)
  (toCNF $ Disjunction prop2 prop3)
toCNF (Disjunction prop1 (Conjunction prop2 prop3)) = toCNF $ Conjunction
  (toCNF $ Disjunction prop1 prop2)
  (toCNF $ Disjunction prop1 prop3)
toCNF (Disjunction prop1 prop2) = Disjunction (toCNF prop1) (toCNF prop2)
-- Implication: disjunction equivalence
toCNF (Implication prop1 prop2) =
  toCNF $ Disjunction (toCNF $ Negation prop1) (toCNF prop2)
-- Negation: Double negation and De Morgan's Laws
toCNF (Negation (Negation prop)) = toCNF prop
toCNF (Negation (Conjunction prop1 prop2)) =
  toCNF $ Disjunction (toCNF $ Negation prop1) (toCNF $ Negation prop2)
toCNF (Negation (Disjunction prop1 prop2)) =
  toCNF $ Conjunction (toCNF $ Negation prop1) (toCNF $ Negation prop2)
toCNF (Negation prop) = toCNF (Negation $ toCNF prop)

data Literal = Positive Char | Negative Char deriving (Eq, Show, Ord)
type Clause = [Literal]
type CNF = [Clause]

complementary :: Literal -> Literal
complementary (Positive prop) = Negative prop
complementary (Negative prop) = Positive prop

emptyClauseIsReachable :: Proposition -> Bool
emptyClauseIsReachable prop = elem [] (clausePropagation $ getClauses prop)

clausePropagation :: CNF -> CNF
clausePropagation [] = []
clausePropagation (clause : cs) =
  clause : (clausePropagation $ cs ++ (propagate clause clause cs))

propagate :: Clause -> Clause -> CNF -> CNF
propagate []             _      clauses = clauses
propagate (literal : ls) clause clauses = propagate
  ls
  clause
  [ resolve literal clause cs | cs <- clauses, elem (complementary literal) cs ]

resolve :: Literal -> Clause -> Clause -> Clause
resolve literal clause complementaryClause =
  removeRepetition
    $  (delete literal clause)
    ++ (delete (complementary literal) complementaryClause)

removeRepetition :: Ord a => [a] -> [a]
removeRepetition [] = []
removeRepetition (x : xs) | elem x xs = removeRepetition xs
                          | otherwise = x : removeRepetition xs

getClauses :: Proposition -> CNF
getClauses (Disjunction prop1 prop2) =
  [removeRepetition $ getLiterals prop1 ++ getLiterals prop2]
getClauses (Conjunction prop1 prop2) = getClauses prop1 ++ getClauses prop2
getClauses prop                      = [removeRepetition $ getLiterals prop]

getLiterals :: Proposition -> Clause
getLiterals (Atomic   c             ) = [Positive c]
getLiterals (Negation (Atomic c)    ) = [Negative c]
getLiterals (Disjunction prop1 prop2) = getLiterals prop1 ++ getLiterals prop2
