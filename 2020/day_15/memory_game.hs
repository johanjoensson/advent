import System.IO
import Data.List.Split
import Data.Map (Map, empty, member, insert, (!))

type Round = Int
type Memory = Map Int Round

next_number :: Memory -> (Round, Int) -> (Int, Memory)
next_number memory (previous_round, previous_number)
    | previous_number `member` memory = (round_diff, new_memory)
    | otherwise = (0, new_memory)
        where new_memory = insert previous_number previous_round memory
              round_diff = previous_round - memory ! previous_number

insert_init :: Memory -> Int -> [Int] -> Memory
insert_init memory _ [] = memory
insert_init memory round (num:rest) = insert_init new_memory (round + 1) rest
    where new_memory = insert num round memory

get_number :: [Int] -> Int -> Int
get_number init n
                | n < (length init) = init !! n
                | otherwise = go (insert_init empty 0 init) (length init) (last init)
                where go memory round previous
                                    | round == n = new_val
                                    | otherwise = go new_memory (round + 1) new_val
                                    where (new_val, new_memory) = next_number memory (round - 1, previous)
main = do
    contents <- readFile "starting.txt"
    let initials = map (\i -> read i ::Int) . splitOn "," . head . filter ((0 <) . length) . lines $ contents
    putStrLn "Read this number of initial numbers"
    print . length $ initials
    let start = insert_init empty 0 initials
    putStrLn "2020th number in the sequence is"
    print . get_number initials $ 2019
    putStrLn "30000000th number in the sequence is"
    print . get_number initials $ 29999999
