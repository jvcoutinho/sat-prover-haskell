module Resolution (proof) where

import           Proposition
import           Data.List

proof :: Proposition -> Bool
proof prop = clausePropagation . getClauses . toCNF $ Negation prop

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
toCNF (Disjunction prop1 prop2 ) = Disjunction (toCNF prop1) (toCNF prop2)
-- Implication: disjunction equivalence
toCNF (Implication prop1 prop2 ) = toCNF $ Disjunction (Negation prop1) prop2
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

clausePropagation :: CNF -> Bool
clausePropagation []   = False
clausePropagation [[]] = True --found empty clause
clausePropagation (clause : cs) =
  clausePropagation $ literalPropagation clause cs

literalPropagation :: Clause -> CNF -> CNF
literalPropagation [] clauses = clauses
literalPropagation (literal : ls) clauses =
  literalPropagation ls $ propagate literal clauses

propagate :: Literal -> CNF -> CNF
propagate literal clauses =
  differentLiterals ++ (deleteLiteral $ complementaryLiterals differentLiterals) where

  differentLiterals     = filter (\clause -> not $ elem literal clause) clauses

  complementaryLiterals = filter (elem $ complementary literal)

  deleteLiteral         = map (delete $ complementary literal)

getClauses :: Proposition -> CNF
getClauses (Disjunction prop1 prop2) = [getLiterals prop1 ++ getLiterals prop2]
getClauses (Conjunction prop1 prop2) = getClauses prop1 ++ getClauses prop2
getClauses prop                      = [getLiterals prop]

getLiterals :: Proposition -> Clause
getLiterals (Atomic   c             ) = [Positive c]
getLiterals (Negation (Atomic c)    ) = [Negative c]
getLiterals (Disjunction prop1 prop2) = getLiterals prop1 ++ getLiterals prop2
