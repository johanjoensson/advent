import System.IO
import Data.Char

data Operand = Add | Sub | Mult deriving (Show)
type Val = Int
type Expression = Val -> Val

apply_op :: Int -> Operand -> Expression
apply_op n Add = (n +)
apply_op n Sub = (n -)
apply_op n Mult = (n*)



run_calc :: String -> Int
run_calc = f
    where
    f :: String -> [Expression]
    f [] = []
    f ()
        | token == '+' = (+)


calc :: String -> Int
calc s = run 0 s
    where
    run :: Int -> String -> Int
    run acc [] = acc
    run acc (token:rest)
        | token == ')' = run val other
        | token == '+' = acc + run 0 rest
        | token == '-' = (-acc) + run 0 rest
        | token == '*' = acc * run 0 rest
        | token `elem` ['0'..'9'] = run (digitToInt token) rest
        | otherwise = 0
        where (val, other) = subexpression 0 rest
    subexpression :: Int -> String -> (Int, String)
    subexpression acc (token:rest)
        | token == ')' = subexpression val other
        | token == '(' = (acc, rest)
        | token == '+' = (acc + val, other)
        | token == '-' = (val - acc, other)
        | token == '*' = (acc * val, other)
        | token `elem` ['0'..'9'] = subexpression (digitToInt token) rest
        | otherwise = (0,[])
        where (val, other) = subexpression 0 rest
calc2 :: String -> Int
calc2 s = run 0 s
    where
    run :: Int -> String -> Int
    run acc [] = acc
    run acc (token:rest)
        | token == ')' = run val other
        | token == '+' = acc + run 0 rest
        | token == '-' = (-acc) + run 0 rest
        | token == '*' = acc * run 0 rest
        | token `elem` ['0'..'9'] = run (digitToInt token) rest
        | otherwise = 0
        where (val, other) = subexpression 0 rest
    subexpression :: Int -> String -> (Int, String)
    subexpression acc (token:rest)
        | token == ')' = subexpression val other
        | token == '(' = (acc, rest)
        | token == '+' = (acc + val, other)
        | token == '-' = (val - acc, other)
        | token == '*' = (acc * val, other)
        | token `elem` ['0'..'9'] = subexpression (digitToInt token) rest
        | otherwise = (0,[])
        where (val, other) = subexpression 0 rest

main = do
    contents <- readFile "input.txt"
    let ls =  map reverse . map (filter (/= ' ' )) . filter ((>0) . length) . lines $ contents
    print . run_calc $ "123"
