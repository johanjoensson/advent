import System.IO
import Data.List.Split
import Data.List

type BusID = Int

get_bus_schedule :: Int -> [String] -> [(Int, Int)]
get_bus_schedule _ [] = []
get_bus_schedule offset (bus:rest)
				 | bus == "" = get_bus_schedule offset rest
			         | bus == "x" = get_bus_schedule (offset + 1) rest
				 | otherwise = ((busid - offset) `mod` busid, busid) : get_bus_schedule (offset + 1) rest
				 where busid = read bus ::Int

get_wait_time :: Int -> [BusID] -> [(BusID, Int)]
get_wait_time _ [] = []
get_wait_time departure_time (busid:rest) = (busid,wait) : get_wait_time departure_time rest
					where wait = busid - (departure_time `mod` busid)

lcm2 :: [(Int,Int)] -> Int
lcm2 [] = 0
lcm2 pairs
	| all_equal = fst . head $  pairs
	| otherwise = lcm2 $ (n + d, d) : rest
	where all_equal = all (\(i, _) -> i == n) pairs
	      new_pairs = sortOn (fst) pairs
	      (n,d) = head new_pairs
	      rest = tail new_pairs


chinese_remainder :: [(Int, Int)] -> Int
chinese_remainder [] = 0
chinese_remainder ((x,_):[]) = x
chinese_remainder ((xi, di):(xj, dj) : rest)
				      | xi `mod` dj == xj = chinese_remainder $ (xi, di*dj) : rest
				      |otherwise = chinese_remainder $ (xi + di, di) : (xj, dj) : rest


main = do
	contents <- readFile "timelines.txt"
	let bus_schedule = reverse . sortOn (snd) . get_bus_schedule 0  . splitOn "," . last . filter ((0 <) . length) . lines $ contents
	let earliest_departure = (\s -> read s ::Int) . head . filter ((0 <) . length) . lines $ contents
	let buses = sort . map (\s -> read s ::Int) . filter ("x"/=) . splitOn "," . last . filter ((0 <) . length) . lines $ contents
	putStrLn "Number of buses"
	print . length $  buses
	putStrLn "Earliest departure time"
	print earliest_departure
	let wait_times = sortOn (snd) . get_wait_time earliest_departure $ buses
	let min_wait = head [id * time | (id,time) <- wait_times]
	putStrLn "Minimum wait time multiplied with busId, is"
	print min_wait
	putStrLn "Earliest departure time with all bus lines departing in sequence"
	print $ chinese_remainder $ bus_schedule

