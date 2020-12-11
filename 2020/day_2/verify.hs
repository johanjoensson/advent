import System.IO
import Control.Monad
import Data.List.Split

count_chars :: Char -> [Char] -> Int -> Int
count_chars _ [] acc = acc
count_chars char (c:cs) acc
	|c == char = count_chars char cs (acc + 1)
	|otherwise = count_chars char cs acc

check_toboggan_policy :: Int -> Int -> Char -> [Char] -> Bool
check_toboggan_policy i j char pwd 
	|pwd !! (i - 1) == char && pwd !! (j - 1) /= char = True
	|pwd !! (i - 1) /= char && pwd !! (j - 1) == char = True
	|otherwise = False

check_policy :: Int -> Int -> Char -> [Char] -> Bool
check_policy min max char pwd 
	|n >= min && n <= max = True
	|otherwise = False
	where n = count_chars char pwd 0
	

verify_toboggan :: String -> Bool
verify_toboggan  [] = False
verify_toboggan str = check_toboggan_policy min max char pwd
        where   min = read ((splitOn "-" (w !! 0)) !! 0)
        	max = read ((splitOn "-" (w !! 0)) !! 1)
                char = splitOn ":" (w !! 1) !! 0 !! 0
                pwd = (w !! 2)
		w = splitOn " " str

verify :: String -> Bool
verify  [] = False
verify str = check_policy min max char pwd
        where   min = read ((splitOn "-" (w !! 0)) !! 0)
        	max = read ((splitOn "-" (w !! 0)) !! 1)
                char = splitOn ":" (w !! 1) !! 0 !! 0
                pwd = (w !! 2)
		w = splitOn " " str

count_matches :: Int -> [String] -> Int
count_matches acc [] = acc
count_matches acc (pwd : pwds)
	| verify pwd = count_matches (acc + 1) pwds
	| otherwise = count_matches acc pwds

count_toboggan_matches :: Int -> [String] -> Int
count_toboggan_matches acc [] = acc
count_toboggan_matches acc (pwd : pwds)
	| verify_toboggan pwd = count_toboggan_matches (acc + 1) pwds
	| otherwise = count_toboggan_matches acc pwds

main =  do
        passwords <- readFile "passwords.txt"
	putStrLn "Number of passwords matching regular rules"
	print $ count_matches 0 $ lines passwords
	putStrLn "Number of passwords matching Toboggan rules"
	print $ count_toboggan_matches 0 $ lines passwords
