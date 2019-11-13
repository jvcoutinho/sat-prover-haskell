module Main where

import           Resolution
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
        TruthTable -> putStrLn "Truth Table Method"
    proofPropositions (map PropositionParser.prop_parser propositions) method
    hClose hFile

proofPropositions :: [Proposition] -> Method -> IO ()
proofPropositions []          _      = return ()
proofPropositions (prop : ps) method = do
    putStrLn $ (Main.proof prop method)
    proofPropositions ps method

proof :: Proposition -> Method -> String
proof prop method
    | case method of
        Resolution -> Resolution.proof prop
        TruthTable -> TruthTable.proof prop
    = show prop ++ " is tautology."
    | otherwise
    = show prop ++ " is not tautology."


readLines :: Handle -> IO [String]
readLines file = do
    isEOF <- hIsEOF file
    if isEOF then return [] else liftM2 (:) (hGetLine file) (readLines file)

data Method = Resolution | TruthTable | All deriving (Show, Read)
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
                    "The method that will solve the problem(s)."
                )
