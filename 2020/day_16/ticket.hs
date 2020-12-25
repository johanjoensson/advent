import System.IO
import Data.List.Split
import Data.List (delete)
import Data.Map (Map, empty, insert, elems, keys, toList)
import qualified Data.Map as Mp (map, delete, filter, difference, union, intersection, null, foldr)

type Field = String
type Rule = Int -> Bool

type Ticket = [Int]

parse_ticket :: String -> Ticket
parse_ticket s = fields
    where fields = map (\i -> read i ::Int) . splitOn "," $ s

parse_rule :: String -> (Field, Rule)
parse_rule s = (key, (\i -> any (i `elem`) ranges))
    where   key = head sl
            ranges = map (range) lus
            range (lower, upper) = [lower .. upper]
            lus = map (\l -> (read (head l)::Int, read (last l)::Int)) . map (splitOn "-") $  ranges_strings
            ranges_strings = (splitOn "or" (last sl))
            sl = splitOn ":" s

get_nearby :: [String] -> ([Ticket], [String])
get_nearby [] = ([], [])
get_nearby (line:rest)
    | valid = (parse_ticket line : nearbies, [])
    | otherwise = (nearbies, rest)
    where   valid = (head line) `elem` ['0' .. '9']
            (nearbies, _) = get_nearby rest

parse_lines :: [String] -> (Ticket, [Ticket], Map String Rule)
parse_lines lines = parser lines ([], [[]], empty)
    where   parser :: [String] -> (Ticket, [Ticket], Map Field Rule) -> (Ticket, [Ticket], Map Field Rule)
            parser [] res = res
            parser ("your ticket:":line:rest) (_, others, rules) = parser rest (parse_ticket line, others, rules)
            parser ("nearby tickets:":rest) (my_ticket, _, rules)  = parser remainder (my_ticket, nearbies, rules)
                where (nearbies, remainder) = get_nearby rest
            parser (line:rest) (my_ticket, others, rules) = parser rest (my_ticket, others, insert field rule rules)
                where (field, rule) = parse_rule line

validate_ticket :: [Rule] -> Ticket -> Bool
validate_ticket rules numbers = all (\i -> any (\r -> r i) rules) numbers

sum_invalid :: [Rule] -> [Ticket] -> Int
sum_invalid _ [] = 0
sum_invalid rules (ticket:rest) = sum [x | x <- ticket, not. any (\f -> f x) $ rules] + sum_invalid rules rest

check_fields :: Rule -> [Ticket] -> [Bool]
check_fields rule tickets
    | head tickets == [] = []
    | otherwise = (all rule (map head tickets)) : check_fields rule (map (drop 1) tickets)


find_fields :: [(Field, Rule)] -> [Ticket] -> Map Field [Int]
find_fields [] _ = empty
find_fields ((field, rule):rest) tickets = insert field pos $ find_fields rest tickets
    where   find_pos :: Rule -> [Ticket] -> [Int]
            find_pos rule tickets = [i | i <- [0 .. length (head tickets) - 1], (check_fields rule tickets) !! i]
            pos = find_pos rule tickets

remove_position :: Int -> Map Field [Int] -> Map Field [Int]
remove_position pos m = Mp.map (delete pos) m

find_unique_fields :: Map Field [Int] -> Map Field [Int]
find_unique_fields = Mp.filter ((1 ==) . length)

remove_fields :: [Field] -> Map Field [Int] -> Map Field [Int]
remove_fields [] m = m
remove_fields (field:rest) m = Mp.delete field $ remove_fields rest m

find_unique_positions :: Map Field [Int] -> Map Field Int
find_unique_positions m = go empty m
    where go :: Map Field Int -> Map Field [Int] -> Map Field Int
          go res m
            | Mp.null m = res
            | otherwise = go (solved) (slimmed)
            where solved = Mp.union res . Mp.map (head) . find_unique_fields $ m
                  flds = keys $ find_unique_fields m
                  els = elems . Mp.map (head) $ find_unique_fields m
                  slimmed = remove_fields flds . foldr (Mp.intersection) m . map  (\f -> f m) $ map (remove_position) els

starts_with_departure :: String -> Bool
starts_with_departure s
    | (head . words $ s) == "departure" = True
    | otherwise = False

main = do
    contents <- readFile "input.txt"
    let ls = filter ((0<) . length) . lines $ contents
    putStrLn "Lines read"
    print . length $ ls
    let (my_ticket, others, rules) = parse_lines ls
    putStrLn "Number of nearby tickets"
    print . length $ others
    putStrLn "Sum of all fields that match no rule"
    print . sum_invalid (elems rules) $ others
    --print . filter (validate_ticket (elems rules)) $ others
    --print . check_fields (head . elems $ rules) . filter (validate_ticket . elems $ rules) $ others
    let u_pos = find_unique_positions . find_fields (toList rules) . filter (validate_ticket (elems rules)) $ others
    putStrLn "Product of all departure fields is"
    print $ product [my_ticket !! i | (field, i) <- (toList u_pos), starts_with_departure field]
