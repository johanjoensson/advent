import System.IO
import Data.Maybe

data Instruction = NOP Int | JMP Int | ACC Int deriving (Eq,Show)

data CPU = CPU { acc ::Int, pc ::Int } deriving (Eq,Show)

get_instruction line 
		    | inst == "nop" = Just (NOP num)
		    | inst == "jmp" = Just (JMP num)
		    | inst == "acc" = Just (ACC num)
		    | otherwise = Nothing
		    where w = words line
		    	  inst = head w
			  num | sign == '+' = read $ drop 1 (w !! 1)
			      | otherwise = read $ w !! 1
			  sign = head $ w !! 1

parse_instructions [] = []
parse_instructions ("":rest) = parse_instructions rest
parse_instructions (line:rest)
			     | instruction == Nothing = parse_instructions rest
			     | otherwise = fromJust instruction : parse_instructions rest
			     where instruction = get_instruction line

execute_instruction (CPU {acc = a, pc = i}) (NOP _) = CPU {acc = a, pc = i + 1}
execute_instruction (CPU {acc = a, pc = i}) (JMP n) = CPU {acc = a, pc = i + n}
execute_instruction (CPU {acc = a, pc = i}) (ACC n) = CPU {acc = a + n, pc =  i + 1}

check_loop boot executed_instructions cpu
					| pc cpu >= length boot = (0, False)
					| pc new_cpu `elem` executed_instructions = (acc new_cpu, True)
					| otherwise = check_loop boot (pc cpu : executed_instructions) new_cpu
					where new_cpu = execute_instruction cpu (boot !! pc cpu)
					
modify_instructions :: ([Instruction], [Instruction]) -> [([Instruction], [Instruction])]
modify_instructions (new_set, []) = (new_set, []):[]
modify_instructions (new_set, ((NOP n):instructions)) 
					   = (new_set ++ (new_jmp:instructions), []) : (modify_instructions (new_set ++ ((NOP n):[]), instructions))
					   where new_jmp = (JMP n)
modify_instructions (new_set, ((JMP n):instructions)) 
					   = (new_set ++ (new_nop:instructions), []) : (modify_instructions (new_set ++ ((JMP n):[]), instructions))
					   where new_nop = (NOP n)
modify_instructions (new_set, (current:instructions)) 
					   = modify_instructions (new_set ++ current:[], instructions)

follow_boot instructions cpu
			   | pc cpu >= length instructions = acc cpu
			   | otherwise = follow_boot instructions new_cpu
			   where new_cpu = execute_instruction cpu (instructions !! pc cpu)

main = do
	content <- readFile "boot.txt"
	let instructions = parse_instructions (lines content)
	putStrLn "Number of instructions"
	print $ length instructions
	putStrLn "just before the infinite loop, Acc contains"
	print $ fst $ check_loop instructions [] (CPU {acc = 0, pc = 0})
	putStrLn "Final value of Acc for the first modified boot sequence to terminate"
	print $ head [ follow_boot sequence (CPU {acc = 0, pc = 0}) | (sequence, _) <- modify_instructions ([], instructions), not$ snd $check_loop sequence [] (CPU {acc = 0, pc = 0})]

