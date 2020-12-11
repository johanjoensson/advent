import System.IO
import Data.Map (Map, insert, empty, member, (!), size, keys)
import Data.List.Split
import Data.List (nub, intersect)


get_contents :: [String] -> [(Int, String)]
get_contents [] = []
get_contents w
	      | head w == "bag," = (num (w !! 1), w !! 2 ++ " " ++ w !! 3) : get_contents (drop 4 w)
	      | head w == "bags," = (num (w !! 1), w !! 2 ++ " " ++ w !! 3) : get_contents (drop 4 w)
	      | head w == "contain" && w !! 1 /= "no" = (num (w !! 1), w !! 2 ++ " " ++ w !! 3) : get_contents (drop 4 w)
	      | otherwise = []
	      where num str = read str ::Int


extract_rule :: String -> (String, [(Int, String)])
extract_rule rule_string = (key, l)
			 where key = w !! 0 ++ " " ++ w !! 1
			       w = words rule_string
			       l = get_contents $ drop 3 w

parse_rules luggage_rules [] = luggage_rules
parse_rules luggage_rules (rule:[""]) = insert key val luggage_rules
				    where (key,val) = extract_rule rule
parse_rules luggage_rules (rule:rest) = parse_rules new_rules rest
				      where new_rules = insert key val luggage_rules
				            (key, val) = extract_rule rule

find_key_in_rules :: String -> Map String [(Int, String)] -> [(Int, String)] -> Bool
find_key_in_rules _ _ [] = False
find_key_in_rules search_key luggage_rules ((n, key):other) = search_key == key || find_key_in_rules search_key luggage_rules contents || find_key_in_rules search_key luggage_rules other 
						       where contents = luggage_rules ! key

count_contained_bags luggage_rules [] = 0
count_contained_bags luggage_rules ((n, key):other) = n * (1 + count_contained_bags luggage_rules content) + count_contained_bags luggage_rules other
						    where content = luggage_rules ! key

main = do
        content <- readFile "luggage_rules.txt"
        let rules = lines content
	let m = parse_rules empty rules
	putStrLn "Total number of bags that can contain a shiny gold bag"
	print $ length $ [key | key <- (keys m), key /= "shiny gold", find_key_in_rules "shiny gold" m (m ! key)]
	putStrLn "A single shiny gold bag contains"
	print $ count_contained_bags m (m ! "shiny gold")
	putStrLn "bags"
