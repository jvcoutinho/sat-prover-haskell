module Proposition where

import           Test.QuickCheck
import           Control.Monad
import Control.Applicative
import Data.Char

data Proposition = Atomic Char
                 | Negation Proposition
                 | Conjunction Proposition Proposition
                 | Disjunction Proposition Proposition
                 | Implication Proposition Proposition deriving (Eq)

instance Show Proposition where
  show (Atomic c) = [c]
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
        [ fmap Atomic arbitrary
        , fmap Negation subexpression
        , liftM2 Conjunction subexpression subexpression
        , liftM2 Disjunction subexpression subexpression
        , liftM2 Implication subexpression subexpression]
        where
          subexpression = proposition (n `div` 2)

-- Proposition Parser

prop_parser :: String -> Proposition
prop_parser xs = case (parse expr xs) of
                      [(n,[])] -> n
                      [(_,out)] -> error ("Input not used " ++ out)
                      [] -> error "Invalid input"

-- Implementation of parser

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) cs = p cs

item :: Parser Char
item = P (\cs -> case cs of
                    [] -> []
                    (x:xs) -> [(x, xs)])

instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\cs -> case parse p cs of
                              [] -> []
                              [(a, cs')] -> [(g a, cs')])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\cs -> [(a, cs)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\cs -> case parse pg cs of
                               [] -> []
                               [(g, cs')] -> parse (fmap g px) cs')

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\cs -> case parse p cs of
                             [] -> []
                             [(a, cs')] -> parse (f a) cs')

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\cs -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\cs -> case parse p cs of
                             [] -> parse q cs
                             [(a, cs')] -> [(a, cs')])

-- 

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

letter :: Parser Char
letter = sat isAlpha

char :: Char -> Parser Char
char x = sat (== x)

--

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

symbol :: String -> Parser String
symbol xs = token (string xs)

--

atom :: Parser Proposition
atom = do x <- letter
          return (Atomic x)

_atom :: Parser Proposition
_atom = do char '~'
           a <- atom
           return (Negation a)
           <|> atom

atomic :: Parser Proposition
atomic = token _atom

--

expr :: Parser Proposition
expr = do d <- disjunction
          return d
          <|> empty         

disjunction :: Parser Proposition
disjunction = do c <- conjunction
                 do symbol "+"
                    e <- disjunction 
                    return (Disjunction c e)
                    <|> return c

conjunction :: Parser Proposition
conjunction = do i <- implication
                 do symbol "."
                    c <- conjunction
                    return (Conjunction i c)
                    <|> return i

implication :: Parser Proposition
implication = do f <- factor
                 do symbol ">"
                    i <- implication
                    return (Implication f i)
                    <|> return f

factor :: Parser Proposition
factor = do symbol "("
            e <- disjunction
            symbol ")"
            return e
            <|> atomic
