import System.IO

type Position = (Int, Int)
data Heading = East | North | West | South deriving(Eq, Show)
data Rotation = Clockwise | Anticlockwise deriving(Eq, Show)

data Boat = Boat {position ::Position, heading ::Heading} deriving(Show)
data Instruction = Move (Heading, Int) |Rotate (Rotation, Int) | Forward Int | No deriving (Show)

get_instructions :: [String] -> [Instruction]
get_instructions [] = []
get_instructions ("":rest) = get_instructions rest
get_instructions (line:rest) = parse_line line : get_instructions rest
			     where 
			       parse_line line
				| head line == 'E' = Move (East, num)
				| head line == 'N' = Move (North, num)
				| head line == 'W' = Move (West, num)
				| head line == 'S' = Move (South, num)
				| head line == 'L' = Rotate (Anticlockwise, num `div` 90)
				| head line == 'R' = Rotate (Clockwise, num `div` 90)
				| head line == 'F' = Forward num
				| otherwise = No
				where num = read . tail $ line ::Int

rotate :: Rotation -> Heading -> Heading
rotate Clockwise East = South
rotate Clockwise North = East
rotate Clockwise West = North
rotate Clockwise South = West
rotate Anticlockwise East = North
rotate Anticlockwise North = West
rotate Anticlockwise West = South
rotate Anticlockwise South = East

apply_instruction :: Instruction -> Boat -> Boat
apply_instruction (Move (East,n)) b = Boat {position = (bx + n, by), heading = h}
		    where (bx, by) = position b
			  h = heading b
apply_instruction (Move (North, n)) b = Boat {position = (bx, by + n), heading = h}
		    where (bx, by) = position b
			  h = heading b
apply_instruction (Move (West, n)) b = Boat {position = (bx - n, by), heading = h}
		    where (bx, by) = position b
			  h = heading b
apply_instruction (Move (South, n)) b = Boat {position = (bx, by - n), heading = h}
		    where (bx, by) = position b
			  h = heading b
apply_instruction (Rotate (Clockwise, 1)) b = Boat {position = (bx, by), heading = rotate Clockwise h}
		    where (bx, by) = position b
			  h = heading b
apply_instruction (Rotate (Anticlockwise, 1)) b = Boat {position = (bx, by), heading = rotate Anticlockwise h}
		    where (bx, by) = position b
			  h = heading b
apply_instruction (Forward n) b = apply_instruction (Move (heading b, n)) $ b
apply_instruction (Rotate (d, n)) b = apply_instruction (Rotate (d, 1)) . apply_instruction (Rotate (d, n - 1)) $ b
apply_instruction No b = b
	
apply_instructions :: [Instruction] -> Boat -> Boat
apply_instructions [] b  = b
apply_instructions (i:rest) b = apply_instructions rest (new_boat)
			      where new_boat = apply_instruction i b
			  
apply_instruction2 :: Instruction -> (Boat, Boat) -> (Boat, Boat)
apply_instruction2 (Move (East,n)) (b, t) = (b, Boat {position = (tx + n, ty), heading = h})
		    where (tx, ty) = position t
			  h = heading t
apply_instruction2 (Move (North, n)) (b, t) = (b, Boat {position = (tx, ty + n), heading = h})
		    where (tx, ty) = position t
			  h = heading t
apply_instruction2 (Move (West, n)) (b, t) = (b, Boat {position = (tx - n, ty), heading = h})
		    where (tx, ty) = position t
			  h = heading t
apply_instruction2 (Move (South, n)) (b, t) = (b, Boat {position = (tx, ty - n), heading = h})
		    where (tx, ty) = position t
			  h = heading t
apply_instruction2 (Rotate (Clockwise, 1)) (b, t) = (b, Boat {position = (ty, -tx), heading = rotate Clockwise h})
		    where (tx, ty) = position t
			  h = heading t
apply_instruction2 (Rotate (Anticlockwise, 1)) (b, t) = (b , Boat {position = (-ty, tx), heading = rotate Anticlockwise h})
		    where (tx, ty) = position t
			  h = heading t
apply_instruction2 (Forward n) (b, t) = (Boat {position = (bx + n*dx, by + n*dy), heading = heading b}, t)
		    where (bx, by) = position b
			  (dx, dy) = position t
apply_instruction2 (Rotate (d, n)) (b, t) = apply_instruction2 (Rotate (d, 1)) . apply_instruction2 (Rotate (d, n - 1)) $ (b, t)
apply_instruction2 No (b, t) = (b, t)
			  
apply_instructions2 :: [Instruction] -> (Boat, Boat) -> (Boat, Boat)
apply_instructions2 [] (b, w)  = (b, w)
apply_instructions2 (i:rest) (b, w) = apply_instructions2 rest (new_boat, new_waypoint)
			      where (new_boat, new_waypoint) = apply_instruction2 i (b, w)

manhattan :: Position -> Int
manhattan (x, y) = abs x + abs y

boat_dist :: Boat -> Int
boat_dist b = manhattan . position $ b

main = do
	contents <- readFile "directions.txt"
	let ls = filter ((0 <) . length) . lines $ contents
	let instructions = get_instructions ls
	putStrLn "Boats! Boats! Boats!"
	putStr "Number of instructions = "
	print . length $ instructions
	putStrLn "After following the instructions the boat is here"
	let boat = apply_instructions instructions (Boat {position = (0,0), heading = East})
	print boat
	putStrLn "And the boat has travelled the Manhattan distance"
	print . boat_dist $ boat
	putStrLn "After following the instructions the boat is here"
	let (boat2, _) = apply_instructions2 instructions (Boat {position = (0,0), heading = East}, Boat {position = (10, 1), heading = East})
	print boat2
	putStrLn "And the boat has travelled the Manhattan distance"
	print . boat_dist $ boat2
