module Proposition where

data Proposition = Atomic Char
                 | Negation Proposition
                 | Conjunction Proposition Proposition
                 | Disjunction Proposition Proposition
                 | Implication Proposition Proposition

instance Show Proposition where
    show (Atomic c) = [c]
    show (Negation prop) = "(~" ++ show prop ++ ")"
    show (Conjunction prop1 prop2) = "(" ++ show prop1 ++ " . " ++ show prop2 ++ ")"
    show (Disjunction prop1 prop2) = "(" ++ show prop1 ++ " + " ++ show prop2 ++ ")"
    show (Implication prop1 prop2) = "(" ++ show prop1 ++ " > " ++ show prop2 ++ ")"