module Main where

import Resolution
import Proposition

main :: IO ()
main = do
    putStrLn $ show $ proof (Disjunction (Negation (Atomic 'A')) (Atomic 'A'))
    putStrLn $ show $ proof (Implication (Atomic 'A') (Implication (Atomic 'B') (Atomic 'A')))
    putStrLn $ show $ proof (Negation (Conjunction (Disjunction (Atomic 'A') (Atomic 'A')) (Negation (Disjunction (Atomic 'A') (Atomic 'A')))))
    putStrLn $ show $ proof (Atomic 'A')
