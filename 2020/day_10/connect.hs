import System.IO
import Data.List (sort, nub)

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

check_valid :: Int -> Int -> [Int] -> Bool
check_valid _ _ [] =  False
check_valid start stop list
			   | head list > start + 3 = False
			   | otherwise = all (<= 3) $ get_differences $ (list)
			    

--add_if_valid :: Int -> Int -> Int -> [Int] -> [Int]
add_if_valid max_val _ x [] 
			  | max_val - x <= 3 = x:[]
			  | otherwise = []
add_if_valid _ max_diff x sublist
		      | (head sublist) - x <= max_diff = x:sublist
		      | otherwise = sublist

all_possibilities :: Int -> [Int] -> [[Int]]
all_possibilities _ [] = [[]]
all_possibilities max_val (x:rest) = nub $ sublist ++ (map (add_if_valid max_val 3 x) sublist)
			   where sublist = all_possibilities max_val rest

main = do
	content <- readFile "adapters.txt"
	putStrLn "Number of adapters in bag"
	print . length . get_adapters . lines $ content
	let (ones, threes) = count_occurences [1] [3] . get_differences . sort . get_adapters . lines $ content
	putStrLn "Number of differences of 1 jolt multiplied by number of differences of 3 jolt"
	print  $ (ones + 1) * (threes + 1)
	putStrLn "All valid configurations"
	let adapters = sort . get_adapters . lines $ content
	let end_val = (maximum adapters) + 3
	print . length $ [l | l <-  nub . all_possibilities end_val $ adapters, check_valid 0 end_val l]
