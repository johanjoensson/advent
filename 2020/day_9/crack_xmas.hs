import System.IO

parse_lines [] = []
parse_lines ("":rest) = parse_lines rest
parse_lines (line:rest) = (read line ::Int) : parse_lines rest

rule_breakers _ [] = []
rule_breakers preamble (number:rest)
				   | not valid = number:[]
				   | otherwise = rule_breakers ((drop 1 preamble) ++ number:[]) rest
				   where valid = number `elem` [x + y | x <- preamble, y <- preamble, x /= y]


find_contiguous_sub_list :: Int -> [Int] -> [Int] -> Int
find_contiguous_sub_list target subrange [] 
					    | sum subrange < target = error "Oh no!"
					    |otherwise = find_contiguous_sub_list target subrange [0]

find_contiguous_sub_list target subrange rest
				            | sum subrange == target = maximum subrange + minimum subrange
					    | sum subrange > target = find_contiguous_sub_list target (drop 1 subrange) rest
					    | otherwise = find_contiguous_sub_list target (subrange ++ (head rest):[]) (drop 1 rest)


main = do
	content <- readFile "xmas.txt"
	let numbers = parse_lines . lines $ content
	putStrLn "First number to break the XMAS code"
	print . head $ rule_breakers (take 25 numbers) (drop 25 numbers)
	putStrLn "Sum of maximum and minimum elements in contiguous subrange that sum to above number"
	let target = head $ rule_breakers (take 25 numbers) (drop 25 numbers)
	print $ find_contiguous_sub_list target [] numbers
	putStrLn "Done"
