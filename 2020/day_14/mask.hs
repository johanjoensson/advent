import System.IO
import Data.List.Split
import Data.Map (Map, insert, empty, fold)

data MaskBit = One | Zero | X deriving(Show)

type Adress = Int
type Int36 = Int
data Instruction = SetMask [MaskBit] | Memset (Adress, Int36) deriving(Show)

type Memory = Map Int36 Int36

remove_chars :: [Char] -> String -> String
remove_chars _ [] = []
remove_chars targets (c:rest)
			    | c `elem` targets = remove_chars targets rest
			    | otherwise = c : remove_chars targets rest

read_mask :: String -> Instruction
read_mask s = SetMask (mask s)
	    where mask [] = []
		  mask (c:rest)
			      | c == '1' = One : mask rest
			      | c == '0' = Zero : mask rest
				  | c == 'X' = X : mask rest
			      | otherwise =  mask rest


read_memset :: [String] -> Instruction
read_memset parts = Memset (read_adress . head $ parts, read_memory . last $ parts)
		  where read_adress s = (\l -> read l ::Int) . remove_chars "mem[] " $ s
		  	read_memory s = read s ::Int

parse_line :: String -> Instruction
parse_line line
	      | take 3 line == "mas" =  read_mask . (!! 1) . splitOn "=" $ line
	      | otherwise = read_memset . splitOn "=" $ line

apply_bit_mask :: MaskBit -> Int -> Int
apply_bit_mask One _ = 1
apply_bit_mask Zero _ = 0
apply_bit_mask X bit = bit

apply_bit_mask2 :: MaskBit -> Int -> [Int]
apply_bit_mask2 One _ = [1]
apply_bit_mask2 Zero bit = [bit]
apply_bit_mask2 X _ = [0,1]

apply_mask :: [MaskBit] -> [Int] -> [Int]
apply_mask [] _ = []
apply_mask (mb:mask) [] = apply_bit_mask mb 0 : apply_mask mask []
apply_mask (mb:mask) (bit:rest) = apply_bit_mask mb bit : apply_mask mask rest

insert_bits :: [Int] -> [[Int]] -> [[Int]]
insert_bits [bit] lists = map (bit:) lists
insert_bits (bit:rest) lists = map (bit:) lists ++ insert_bits rest lists

apply_mask2 :: [MaskBit] -> [Int] -> [[Int]]
apply_mask2 [] _ = [[]]
apply_mask2 mask [] =  apply_mask2 mask . repeat $ 0
apply_mask2 (mb:mask) (bit:rest) = insert_bits new_bits . apply_mask2 mask $ rest
								 where new_bits = apply_bit_mask2 mb bit


to_bit :: Int -> [Int]
to_bit 0 = [0]
to_bit i = i `mod` 2 : to_bit (i `div` 2)

from_bit :: [Int] -> Int
from_bit [] = 0
from_bit (bit:rest) = bit + 2 * (from_bit rest)

apply_instruction :: Memory -> [MaskBit] -> Instruction -> (Memory, [MaskBit])
apply_instruction memory mask (SetMask new_mask) = (memory, reverse new_mask)
apply_instruction memory mask (Memset (adress, value)) = (insert adress mask_val memory, mask)
	where mask_val = from_bit . apply_mask mask . to_bit $ value

inserter2 :: [Int] -> Int -> Memory -> Memory
inserter2 [] _ memory = memory
inserter2 (adress:rest) value memory = inserter2 rest value new_memory
	where new_memory = insert adress value memory

apply_instruction2 :: Memory -> [MaskBit] -> Instruction -> (Memory, [MaskBit])
apply_instruction2 memory mask (SetMask new_mask) = (memory, reverse new_mask)
apply_instruction2 memory mask (Memset (adress, value)) = (inserter2 mask_ads value memory, mask)
	where mask_ads = map (from_bit) . apply_mask2 mask . to_bit $ adress

apply_instructions :: Memory -> [MaskBit] -> [Instruction] -> Memory
apply_instructions memory mask [] = memory
apply_instructions memory mask (instruction:rest) = apply_instructions new_mem new_mask rest
												  where (new_mem, new_mask) = apply_instruction memory mask instruction

apply_instructions2 :: Memory -> [MaskBit] -> [Instruction] -> Memory
apply_instructions2 memory mask [] = memory
apply_instructions2 memory mask (instruction:rest) = apply_instructions2 new_mem new_mask rest
												  where (new_mem, new_mask) = apply_instruction2 memory mask instruction

main = do
	contents <- readFile "instructions.txt"
	let instructions = map parse_line . filter ((0 <) . length) . lines $ contents
	putStrLn "I read this many instructions in total "
	print . length $ instructions
	let memory = apply_instructions empty [] instructions
	putStrLn "The sum of all values stored in memory is"
	print . foldr (+) 0 $ memory
	let memory2 = apply_instructions2 empty [] instructions
	putStrLn "The new sum of all values stored in memory is"
	print . foldr (+) 0 $ memory2
