import System.IO
import Data.List (sort, nub)
import Data.Map (Map, insert, empty, (!), member, keys)
import Control.Monad.Fix

get_adapters :: [String] -> [Int]
get_adapters [] = []
get_adapters ("" : rest) = get_adapters rest
get_adapters (str : rest) = (read str :: Int) : get_adapters rest

get_differences (x:y:[]) = y - x : []
get_differences (x:y:rest) = y - x : get_differences (y:rest)

count_occurences _ _ [] = (0,0)
count_occurences a b (x:rest)
			    | x `elem` a = (a_occ + 1, b_occ)
			    | x `elem` b = (a_occ, b_occ + 1)
			    | otherwise = (a_occ, b_occ)
			    where (a_occ, b_occ) = count_occurences a b rest

connect_to :: Map Int [Int] -> [Int] -> Map Int [Int]
connect_to connections [] = connections
connect_to connections (joltage:rest) = connect_to new_connections rest
			    where new_connections = insert joltage connectors connections
				  connectors  = [i | i <- [joltage -3 .. joltage - 1], i `member` connections]

ways_to :: Map Int [Int] -> Int -> Int
ways_to connections joltage
			  | connectors == [] = 1
			  | otherwise = (sum . map (ways_to connections) $ connectors)
			  where connectors = connections ! joltage
				next_joltage = head connectors

memo_ways_to :: Map Int [Int] -> Int -> Int
memo_ways_to connections joltage = (map f [0..]) !! joltage
				where 	f joltage
			  			| connectors == [] = 1
			  			| otherwise = sum . map (memo_ways_to connections) $ connectors
				      		where connectors  = [i | i <- [joltage -3 .. joltage - 1], i `member` connections]


count_blocks :: Int -> [Int] -> [Int]
count_blocks n [] = n:[]
count_blocks n (x:rest)
		    | x `mod` 3 == 1 = count_blocks (n + 1) rest
		    | n > 0 = n : (count_blocks 0 rest)
		    | otherwise = count_blocks 0 rest

main = do
	content <- readFile "adapters.txt"
	putStrLn "Number of adapters in bag"
	print . length . get_adapters . lines $ content
	let (ones, threes) = count_occurences [1] [3] . get_differences . sort . get_adapters . lines $ content
	putStrLn "Number of valid connections"
	let adapters = sort . get_adapters . lines $ content
	let max_val = 3 + maximum adapters
	let connections = connect_to empty (0:adapters++[max_val])
	print $ memo_ways_to connections max_val
