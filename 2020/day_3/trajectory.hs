import System.IO

check_tree topology x y 
	| (topology !! j) !! i == '#' = True
	| otherwise = False
	where i = x `mod` (length $ topology !! 0)
	      j = y `mod` (length topology)

count_trees topology acc x y dx dy 
	| y <= length topology && check_tree topology x y = count_trees topology (acc + 1) (x + dx) (y + dy) dx dy
	| y <= length topology = count_trees topology acc (x + dx) (y + dy) dx dy
	| otherwise = acc

main = do
	content <- readFile "slope.txt"
	let slope = lines content
	putStrLn "Descending with dx = 3, dy = 1"
	let n = count_trees slope 0 0 0 3 1
	putStrLn "Trees encountered"
	print n
	let descender = count_trees slope 0 0 0
	let descents = [[1,1], [3, 1], [5, 1], [7, 1], [1, 2]]
	let res = product [descender x y | (x:y:_) <- descents ]
	putStrLn "Product of trees encountered"
	print res
