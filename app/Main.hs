module Main where

import Resolution
import Proposition
import Parser

main :: IO ()
main = do
    putStrLn $ show $ prop_parser "((~A) + A)"
    putStrLn $ show $ proof $ prop_parser "(A + B) . (A + (~B)) . ((~A) + B) . ((~A) + (~B))"
    putStrLn $ show $ proof (Implication (Atomic 'A') (Implication (Atomic 'B') (Atomic 'A')))
    putStrLn $ show $ proof (Negation (Conjunction (Disjunction (Atomic 'A') (Atomic 'A')) (Negation (Disjunction (Atomic 'A') (Atomic 'A')))))
    putStrLn $ show $ proof (Atomic 'A')
