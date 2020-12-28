import System.IO
import Data.Function (fix)
import Data.MemoTrie (memo, memoFix)
import Data.Maybe
import Data.List (find, intersect)
import Data.Map (Map, fromList, insert, empty, lookup, (!))
import qualified Data.Map as Mp

data State = Active | Inactive deriving (Eq, Show)

type X = Int
type Y = Int
type Z = Int
type W = Int

type Position = (X, Y)
type Position3 = (X, Y, Z)
type Position4 = ((X, Y, Z), W)
type Pos4 = (X, Y, Z, W)

type Grid = Position -> State
type Grid3 = Position3 -> State
type Grid4 = Position4 -> State


count :: Eq a => a -> [a] -> Int
count x = length . filter ( == x)

state_char :: State -> String
state_char Active = "#"
state_char Inactive = "."

grid_string ::(X, Y, Z) -> Grid3 -> String
grid_string (last_row, last_column, last_depth) grid = unlines rows
                     where rows = [unlines [unwords . map state_char $ [grid (x, y, z) | y <- [0 .. last_row]] | x <- [0 .. last_column]] | z <- [-last_depth .. last_depth]]

grid4_string ::(X, Y, Z, W) -> (Pos4 -> State) -> String
grid4_string (last_row, last_column, last_depth, last_time) grid4 = unlines rows
                     where rows = [unlines [unlines [unwords . map state_char $ [grid4 (x, y, z, w) | y <- [0 .. last_row]] | x <- [0 .. last_column]] | z <- [-last_depth .. last_depth]] | w <- [-last_time .. last_time]]

get_initial_config :: [String] -> Grid
get_initial_config lines = memo init_grid
               where
            init_grid (x, y)
              | x < 0 = Inactive
              | y < 0 = Inactive
              | y >= length lines = Inactive
              | x >= length (lines !! y) = Inactive
              | char == '#' = Active
              | char == '.' = Inactive
              | otherwise = Inactive
              where char = (lines !! x) !! y

get_initial_config3 :: [String] -> Grid3
get_initial_config3 lines = memo init_grid
    where
    init_grid (x, y, 0) = get_initial_config lines (x, y)
    init_grid _ = Inactive

get_initial_config4 :: [String] -> Grid4
get_initial_config4 lines = memo init_grid
    where
    init_grid ((x, y, z), 0) = get_initial_config3 lines (x, y, z)
    init_grid _ = Inactive

get_initial_config4' :: [String] -> (Pos4 -> State)
get_initial_config4' lines = init_grid
    where
    init_grid (x, y, z, 0) = get_initial_config3 lines (x, y, z)
    init_grid _ = Inactive

game_of_cubes :: Grid3 -> Int -> Grid3
game_of_cubes initial = curry  (memoFix goc)
             where
             goc f (0, point) = initial point
             goc f (n, point) = step (f (n - 1, point)) (map (\p -> f (n - 1, p)) (neighbors point))
             step :: State -> [State] -> State
             step Inactive ngh
                 | count Active ngh == 3 = Active
                 | otherwise = Inactive
             step Active ngh
                     | count Active ngh == 2 = Active
                     | count Active ngh == 3 = Active
                     | otherwise = Inactive
             neighbors :: Position3 -> [Position3]
             neighbors (x, y, z) = [(x + i, y + j, z + k) | i <- [-1 .. 1], j <- [-1 ..  1], k <- [-1 .. 1], (i, j, k) /= (0, 0, 0)]

goc4d :: [Pos4] -> Int -> [Pos4]
goc4d initial n = run empty 0
    where
    run :: Map Int [Pos4] -> Int -> [Pos4]
    run mem it
        | n == 0 = initial
        | it == 0 = run (fromList [(0, initial)]) (it + 1)
        | it == n = active_points
        | otherwise = run new_mem (it + 1)
        where
        new_mem = insert it active_points mem
        new_grid = state

        xmin = foldr (\(x, _, _, _) xmin -> min xmin x) 0 (new_mem ! (it - 1))
        xmax = foldr (\(x, _, _, _) xmax -> max xmax x) 0 (new_mem ! (it - 1))
        ymin = foldr (\(_, y, _, _) ymin -> min ymin y) 0 (new_mem ! (it - 1))
        ymax = foldr (\(_, y, _, _) ymax -> max ymax y) 0 (new_mem ! (it - 1))
        zmin = foldr (\(_, _, z, _) zmin -> min zmin z) 0 (new_mem ! (it - 1))
        zmax = foldr (\(_, _, z, _) zmax -> max zmax z) 0 (new_mem ! (it - 1))
        wmin = foldr (\(_, _, _, w) wmin -> min wmin w) 0 (new_mem ! (it - 1))
        wmax = foldr (\(_, _, _, w) wmax -> max wmax w) 0 (new_mem ! (it - 1))

        active_points = [(x, y, z, w) | x <- [xmin - 1 .. xmax + 1], y <- [ymin - 1 .. ymax + 1], z <- [zmin - 1 .. zmax + 1], w <- [wmin - 1 .. wmax + 1], state (x, y, z, w) == Active]

        state :: Pos4 -> State
        state pos
            | pos `elem` new_mem ! (it - 1) = step Active $ live_neighbors pos
            | otherwise = step Inactive $ live_neighbors pos

        step :: State -> [Pos4] -> State
        step Inactive ngh
            | length ngh == 3 = Active
            | otherwise = Inactive
        step Active ngh
            | length ngh == 2 = Active
            | length ngh == 3 = Active
            | otherwise = Inactive

        live_neighbors :: Pos4 -> [Pos4]
        live_neighbors (x, y, z, w) = intersect (new_mem ! (it - 1)) [(x + i, y + j, z + k, w + l) | i <- [-1 .. 1], j <- [-1 ..  1], k <- [-1 .. 1], l <- [-1 .. 1], (i, j, k, l) /= (0, 0, 0, 0)]



game_of_cubes_4D :: Grid4 -> Int -> Grid4
game_of_cubes_4D initial n point = memoFix (goc initial) (0, point)
    where
    -- goc :: Grid4 -> ((Int, Position4) -> State) -> State
    goc previous f (it, point)
        | it == n = new_grid point
        | otherwise = memoFix (goc new_grid) ((it + 1), point)
        where
        new_grid point = step (previous point) (map previous $ neighbors point)
        step :: State -> [State] -> State
        step Inactive ngh
            | count Active ngh == 3 = Active
            | otherwise = Inactive
        step Active ngh
            | count Active ngh == 2 = Active
            | count Active ngh == 3 = Active
            | otherwise = Inactive
        neighbors :: Position4 -> [Position4]
        neighbors ((x, y, z), w) = [((x + i, y + j, z + k), w + l) | i <- [-1 .. 1], j <- [-1 ..  1], k <- [-1 .. 1], l <- [-1 .. 1], (i, j, k, l) /= (0, 0, 0, 0), z + k `elem` [-n .. n], w + l `elem` [-n .. n]]

main = do
    contents <- readFile "cubes.txt"
    let ls = filter (\l -> length l > 0) . lines $ contents
    let initial_config = get_initial_config3 . filter ((> 0) . length) . lines $ contents
    let max_row = (length ls) - 1
    let max_column = (length $ ls !! 0) - 1

    putStrLn "Number of active cubes in 3D pocket space"
    let it = 6
    print . count Active . map (game_of_cubes initial_config it) $ [(x, y, z) | x <- [-it .. max_column + it], y <- [-it .. max_row + it], z <- [-it .. it]]

    putStrLn "Number of active cubes in 4D pocket space"
    let initial_config4 = get_initial_config4' . filter ((> 0) . length) . lines $ contents
    let it = 6


    let initial_live = [(x, y, z, w) | x <- [0 .. max_column], y <- [0 .. max_row], z <- [0 .. 0], w <- [0 .. 0], initial_config4 (x, y, z, w) == Active]
    let game = goc4d initial_live
    print . length $ game it
