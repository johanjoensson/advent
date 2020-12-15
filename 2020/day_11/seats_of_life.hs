import System.IO
import Data.Function (fix)
import Data.MemoTrie (memo, memoFix)
import Data.Maybe
import Data.List (find)

type Row = Int
type Column = Int

type Seat = (Row, Column)
data State = Floor | Free | Occupied deriving (Eq, Show)
type Grid = Seat -> State

count :: Eq a => a -> [a] -> Int
count x = length . filter ( == x)

state_char :: State -> String
state_char Floor = "."
state_char Free = "L"
state_char Occupied = "#"

grid_string ::(Row, Column) -> Grid -> String
grid_string (last_row, last_column) grid = unlines rows
					 where rows = [unwords . map state_char $ [grid (r, c) | c <- [0 .. last_column]] | r <- [0 .. last_row]]

get_initial_seating :: [String] -> Grid
get_initial_seating lines = memo init_grid
		       where 
			init_grid (x, y)
			  | x < 0 = Floor
			  | y < 0 = Floor
			  | x >= length lines = Floor
			  | y >= length (lines !! x) = Floor
			  | char == 'L' = Free
			  | char == '#' = Occupied
			  | char == '.' = Floor
			  | otherwise = Floor
			  where char = (lines !! x) !! y

game_of_seat :: Grid -> Int -> Grid
game_of_seat initial = curry  (memoFix gos)
		     where 
		     gos f (0, point) = initial point
		     gos f (n, point) = step (f (n - 1, point)) (map (\p -> f (n - 1, p)) (neighbors point))
		     step :: State -> [State] -> State
		     step Floor _ = Floor
		     step Free ngh 
				 | count Occupied ngh == 0 = Occupied
				 | otherwise = Free
		     step Occupied ngh
				     | count Occupied ngh >= 4 = Free
				     | otherwise = Occupied
		     neighbors :: Seat -> [Seat]
		     neighbors (row, column) = [(row + i, column + j) | i <- [-1 .. 1], j <- [-1 ..  1], (i, j) /= (0, 0)]

game_of_seat2:: (Row, Column) -> Grid -> Int -> Grid
game_of_seat2 (max_row, max_column) initial = curry  (memoFix gos)
		     where 
		     gos f (0, point) = initial point
		     gos f (n, point) = step (f (n - 1, point)) (map (map (\p -> f (n - 1, p))) (neighbors point))
		     step :: State -> [[State]] -> State
		     step Floor _ = Floor
		     step Free ngh 
				 | n_occ ngh == 0 = Occupied
				 | otherwise = Free
		     step Occupied ngh
				     | n_occ ngh >= 5 = Free
				     | otherwise = Occupied
		     n_occ = count Occupied . map extract_visible . visible_seats
		     visible_seats = map (find (\s -> s== Occupied ||  s == Free))
		     extract_visible (Just s) = s
		     extract_visible Nothing = Floor
		     ----------------------
		     --    0    1    2    --
		     --     *   *   *     --
		     --                   --
		     --       * * *       --
		     --                   --
		     --   7 * * O * * 3   --
		     --                   --
		     --       * * *       --
		     --                   --
		     --     *   *   *     --
		     --    6    5    4    --
		     ----------------------
		     neighbors :: Seat -> [[Seat]]
		     neighbors (row, column) = [
						[(row - i, column - i) | i <- [1 .. dist_lu]],    -- Ray 0
						[(row - i, column ) | i <- [1 .. row]],                         -- Ray 1
						[(row - i, column + i) | i <- [1 .. dist_ru]],   -- Ray 2
						[(row, column + i) | i <- [1 .. max_column - column]],                      -- Ray 3
						[(row + i, column + i) | i <- [1 .. dist_dr]],  -- Ray 4
						[(row + i, column) | i <- [1 .. max_row - row]],                         -- Ray 5
						[(row + i, column - i) | i <- [1 .. dist_dl]],   -- Ray 6
						[(row, column - i) | i <- [1 .. column]]                       -- Ray 7
					       ]
					     where dist_lu = minimum [row, column]
						   dist_ru = minimum [row, max_column - column]
						   dist_dr = minimum [max_row - row, max_column - column]
						   dist_dl = minimum [max_row - row, column]

find_steady_state (max_row, max_column) gos = n_steady $	find (\i -> seating (i - 1) == seating i) [1..]
			  where seating n = map  (gos n ) [(r, c) | r <- [0 .. max_row], c <- [0 .. max_column]]
				n_steady (Just n) = n
				n_steady Nothing = -1

main = do
	contents <- readFile "seats.txt"
	let ls = filter (\l -> length l > 0) . lines $ contents
	let initial_seating = get_initial_seating . filter (\l -> length l > 0) . lines $ contents
	let max_row = (length ls) - 1
	let max_column = (length $ ls !! 0) - 1
	let gos0 = game_of_seat initial_seating
	let n = find_steady_state (max_row, max_column) gos0
	putStrLn "Steady state solution reached after this many steps"
	print n
	putStrLn "Steady state solution has this many occupied seats"
	print . count Occupied . map (gos0 n) $ [(r, c) | r <- [0 .. max_row], c <- [0 .. max_column]]
	putStrLn "and the seating arrangement looks like this"
	putStr . grid_string (max_row, max_column) $ gos0 n
	let gos2 = game_of_seat2 (max_row, max_column) initial_seating
	let n2 = find_steady_state (max_row, max_column) gos2
	putStrLn "Steady state solution for part 2 reached after this many steps"
	print n2
	putStrLn "Steady state solution for part 2 has this many occupied seats"
	print . count Occupied . map (gos2 n2) $ [(r, c) | r <- [0 .. max_row], c <- [0 .. max_column]]
	putStrLn "and the seating arrangement looks like this"
	putStr . grid_string (max_row, max_column) $ gos2 n2
