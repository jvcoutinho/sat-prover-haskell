module TruthTable
    ( proof
    )
where

import           Data.Maybe
import           Proposition
import           Data.Set                       ( union
                                                , fromList
                                                , toList
                                                )
import qualified Data.Set                      as Set

type TruthValue = (Char, Bool)

-- Given a proposition, returns if it is a tautology.
proof :: Proposition -> Bool
proof prop =
    not
        $ containsFalseValue
        $ map (\values -> evaluate values prop)
        $ truthTable prop  where

    containsFalseValue :: [Bool] -> Bool
    containsFalseValue = elem False

-- Given a map variable->boolean, evaluates a proposition.
evaluate :: [TruthValue] -> Proposition -> Bool
evaluate table (Atomic var  ) = getTruthValue table var
evaluate table (Negation prop ) = not (evaluate table prop)
evaluate table (Conjunction prop1 prop2) =
    (evaluate table prop1) && (evaluate table prop2)
evaluate table (Disjunction prop1 prop2) =
    (evaluate table prop1) || (evaluate table prop2)
evaluate table (Implication prop1 prop2) =
    (not (evaluate table prop1)) || (evaluate table prop2)

getTruthValue :: [TruthValue] -> Char -> Bool
getTruthValue table var = case lookup var table of
    Just b  -> b
    Nothing -> error "Atomic not found."

-- Given a proposition, returns all of its variables.
getAtomics :: Proposition -> [Char]
getAtomics (Atomic a   ) = [a]
getAtomics (Negation prop) = getAtomics prop
getAtomics (Conjunction prop1 prop2) =
    toList (fromList (getAtomics prop1) `union` fromList (getAtomics prop2))
getAtomics (Disjunction prop1 prop2) =
    toList (fromList (getAtomics prop1) `union` fromList (getAtomics prop2))
getAtomics (Implication prop1 prop2) =
    toList (fromList (getAtomics prop1) `union` fromList (getAtomics prop2))

-- Given an integer n, returns all of the n-possible combinations of booleans.
combinationValues :: Int -> [[Bool]]
combinationValues 0 = [[]]
combinationValues n = do
    boolean <- [True, False]
    map (boolean :) (combinationValues (n - 1))

-- Given a proposition, returns all of the mappings between truth values and variables.
truthTable :: Proposition -> [[TruthValue]]
truthTable prop = mapValues (truthValues prop) (getAtomics prop)  where

    truthValues :: Proposition -> [[Bool]]
    truthValues = combinationValues . length . getAtomics

    mapValues :: [[Bool]] -> [Char] -> [[TruthValue]]
    mapValues values variables =
        map (\valuesList -> zipWith (,) variables valuesList) values
