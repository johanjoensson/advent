import System.IO

get_row _ _ [] = -1
get_row min max (c:[])
			| c == 'F' = min
			| c == 'B' = max
			| otherwise = error ("Unknown character " ++ [c])
get_row min max (c:rest)
			|c == 'F' = get_row min (min + dy - 1) rest
			|c == 'B' = get_row (min + dy) max rest
			|otherwise = error ("Unknown character " ++ [c])
			where dy = (max - min + 1 ) `div` 2 

get_column _ _ [] = -1
get_column min max (c:[])
			| c == 'L' = min
			| c == 'R' = max
                        | otherwise = error ("Unknown character " ++ [c])
get_column min max (c:rest)
			| c == 'L' = get_column min (min + dx - 1) rest
			| c == 'R' = get_column (min + dx) max rest
                        | otherwise = error ("Unknown character " ++ [c])
			where dx = (max - min + 1) `div` 2


get_seat_numbers [""] = []
get_seat_numbers (line:rest) = 8*row + column : get_seat_numbers rest
			       where row = (get_row 1 128 (take 7 line)) - 1
			             column = (get_column 1 8 (drop 7 line)) - 1

find_my_seat row column occupied_seats 
				       | row >= 128 = []
				       | not (current_seat `elem` occupied_seats) && previous_seat `elem` occupied_seats && next_seat `elem` occupied_seats = current_seat : find_my_seat new_row new_column occupied_seats
				       | otherwise = find_my_seat new_row new_column occupied_seats
				       where current_seat = 8*row + column
				             previous_seat = current_seat - 1
					     next_seat = current_seat + 1
					     new_column = (column + 1) `mod` 8
					     new_row | column == 7 = row + 1
					     	     | otherwise = row

my_seat occupied_seats = [8*row + column | row <- [0..127], column <- [0..7], 8*row + column + 1 `elem` occupied_seats, 8*row + column - 1 `elem` occupied_seats, not (8*row + column `elem` occupied_seats)]

main = do
	contents <- readFile "boarding_passes.txt"
	let l = lines contents
	let seat_numbers = get_seat_numbers l
	putStrLn "Highest seat number"
	print $ maximum seat_numbers
	putStrLn "My seat should be"
	let pos = find_my_seat 1 1 seat_numbers
	print $ head pos
