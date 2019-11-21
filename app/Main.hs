module Main where

import           Resolution
import           Tableaux
import           TruthTable
import           Proposition
import           Options.Applicative
import           System.IO
import           Control.Monad
import           Parser                        as PropositionParser

main :: IO ()
main = run =<< execParser opts
  where
    opts =
        info (argumentParser <**> helper) (fullDesc <> progDesc "SAT Prover")

run (Arguments file method) = do
    hFile        <- openFile file ReadMode
    propositions <- readLines hFile
    case method of
        Resolution -> putStrLn "Resolution Method"
        Tableaux -> putStrLn "Tableaux Method"
        TruthTable -> putStrLn "Truth Table Method"
    proofPropositions (map PropositionParser.prop_parser propositions) method
    hClose hFile

proofPropositions :: [Proposition] -> Method -> IO ()
proofPropositions []          _      = return ()
proofPropositions (prop : ps) method = do
    result <- Main.proof prop method
    putStrLn result 
    putStrLn ""
    proofPropositions ps method

proof :: Proposition -> Method -> IO String
proof prop Resolution = do
    result <- Resolution.proof prop
    if result
        then return $ show prop ++ " is tautology."
        else return $ show prop ++ " is not tautology."

proof prop Tableaux = let
    result = Tableaux.proof prop
    in
    if result
        then return $ show prop ++ " is tautology."
        else return $ show prop ++ " is not tautology."

proof prop TruthTable = let
    result = TruthTable.proof prop
    in
    if result
        then return $ show prop ++ " is tautology."
        else return $ show prop ++ " is not tautology."


readLines :: Handle -> IO [String]
readLines file = do
    isEOF <- hIsEOF file
    if isEOF then return [] else liftM2 (:) (hGetLine file) (readLines file)

data Method = Resolution | TruthTable | Tableaux | All deriving (Show, Read)
data Arguments = Arguments FilePath Method deriving (Show)

argumentParser :: Options.Applicative.Parser Arguments
argumentParser =
    Arguments
        <$> strOption
                (  long "file"
                <> short 'f'
                <> metavar "FILE"
                <> help
                       "The input file (containing one proposition per line). The propositions may contain variables [A-Z], operators (+, ., >, ~) and parenthesis."
                )
        <*> option
                auto
                (long "method" <> short 'm' <> metavar "METHOD" <> help
                    "The method that will solve the problem(s) (Resolution, TruthTable or AnalyticTableaux)"
                )
