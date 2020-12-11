import System.IO
import Data.Map (Map, insert, empty, member, (!))
import Data.List.Split

validate_byr :: String -> Bool
validate_byr byr_string = length byr_string == 4 && byr >= 1920 && byr <= 2002
			where byr = read byr_string::Int

validate_iyr :: String -> Bool
validate_iyr iyr_string = length iyr_string == 4 && iyr >= 2010 && iyr <= 2020
			where iyr = read iyr_string::Int

validate_eyr :: String -> Bool
validate_eyr eyr_string = length eyr_string == 4 && eyr >= 2020 && eyr <= 2030
			where eyr = read eyr_string::Int

validate_hgt :: String -> Bool
validate_hgt hgt_string
			| n /= 4 && n /= 5 = False
			| last hgt_string == 'm' = height >= 150 && height <= 193
			| last hgt_string == 'n' = height >= 59 && height <= 76
			| otherwise = False
			where height = read (take (n-2) hgt_string) ::Int
			      n = length hgt_string

validate_hcl :: String -> Bool
validate_hcl hcl_string = head hcl_string == '#' && length hcl_string == 7 && (valid_color $ drop 1 hcl_string)
			where valid_color (c:[]) = check_hex c
			      valid_color (c:rest) = check_hex c && valid_color rest
			      check_hex c = c `elem` ['0'..'9'] || c `elem` ['a'..'f']

validate_ecl :: String -> Bool
validate_ecl ecl_string = ecl_string `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validate_pid :: String -> Bool
validate_pid pid_string = length pid_string == 9 && valid_number pid_string
			where valid_number (c:[]) = c `elem` ['0'..'9']
			      valid_number (c:rest) = c `elem` ['0'..'9'] && valid_number rest

check_id id =   member "byr" id && validate_byr ( id ! "byr") &&
                member "iyr" id && validate_iyr ( id ! "iyr") &&
                member "eyr" id && validate_eyr ( id ! "eyr") &&
                member "hgt" id && validate_hgt ( id ! "hgt") &&
                member "hcl" id && validate_hcl ( id ! "hcl") &&
                member "ecl" id && validate_ecl ( id ! "ecl") &&
                member "pid" id && validate_pid ( id ! "pid")

count_valid acc [] = acc
count_valid acc (id:rest)
        | check_id id = count_valid (acc + 1) rest
        | otherwise = count_valid acc rest

build_map map (key : val : _) = insert key val map

treat_line map [] = map
treat_line map (keyval:rest) = treat_line new_map rest
        where new_map = build_map map $ splitOn ":" keyval


extract_map map [] = map
extract_map map (line:block) = extract_map new_map block
        where new_map = treat_line map $ splitOn " " line

extract_maps [] = []
extract_maps (block:rest) = extract_map empty block : extract_maps rest

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

main = do
        content <- readFile "ids.txt"
        let blocks = build_blocks $ lines content
        let maps = extract_maps blocks
        let res = count_valid 0 maps
        putStrLn "Number of valid passports"
        print res
