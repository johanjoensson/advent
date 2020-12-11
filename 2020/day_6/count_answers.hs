import System.IO
import Data.Map (Map, insert, empty, member, (!))
import Data.List.Split
import Data.List (nub, intersect)

get_answers [] = []
get_answers (line:block) =  nub $ line ++ get_answers block

get_common_answers (line:[]) = line
get_common_answers (line:block) = intersect line $ get_common_answers block

extract_block :: ([String], [String]) -> ([String], [String])
extract_block (block, []) = (block, [])
extract_block (block, "" : []) = (block, [])
extract_block (block, line : "" : rest) = (line:block, rest)
extract_block (block, line : rest) = extract_block (line : block, rest)

build_blocks :: [String] -> [[String]]
build_blocks [] = []
build_blocks lines
        |rest == [] = [block]
        |otherwise = block : build_blocks rest
        where (block, rest) = extract_block ([], lines)

count_positive_answers [] = []
count_positive_answers (block:rest) =  length (answers) : count_positive_answers rest
					where answers = get_answers block

count_common_answers [] = []
count_common_answers (block:rest) =  length (answers) : count_common_answers rest
					where answers = get_common_answers block
main = do
        content <- readFile "answers.txt"
        let blocks = build_blocks $ lines content
        putStrLn "Number of groups"
        print $ length blocks
	putStrLn "Sum of questions answered positive for all groups"
	print $ sum $ count_positive_answers blocks
	putStrLn "Sum of questions answered positive by everyone in a group"
	print $ sum $ count_common_answers blocks
