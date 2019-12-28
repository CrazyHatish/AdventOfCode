program :: [Int]
program = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,9,23,27,2,27,6,31,1,5,31,35,2,9,35,39,2,6,39,43,2,43,13,47,2,13,47,51,1,10,51,55,1,9,55,59,1,6,59,63,2,63,9,67,1,67,6,71,1,71,13,75,1,6,75,79,1,9,79,83,2,9,83,87,1,87,6,91,1,91,13,95,2,6,95,99,1,10,99,103,2,103,9,107,1,6,107,111,1,10,111,115,2,6,115,119,1,5,119,123,1,123,13,127,1,127,5,131,1,6,131,135,2,135,13,139,1,139,2,143,1,143,10,0,99,2,0,14,0]

intcode :: [Int] -> Int -> Int
intcode code pos
    | opcode == 1  = intcode (add code pos1 pos2 pos3) (pos + 4)
    | opcode == 2  = intcode (mult code pos1 pos2 pos3) (pos + 4)
    | opcode == 99 = head code
    | otherwise    = error "Unknown opcode" 
    where opcode = code !! pos
          pos1   = code !! (pos + 1)
          pos2   = code !! (pos + 2)
          pos3   = code !! (pos + 3)

add :: [Int] -> Int -> Int -> Int -> [Int]
add code pos1 pos2 pos3 =
    let (a, b) = (code !! pos1, code !! pos2)
    in take pos3 code ++ [a + b] ++ drop (pos3 + 1) code

mult :: [Int] -> Int -> Int -> Int -> [Int]
mult code pos1 pos2 pos3 =
    let (a, b) = (code !! pos1, code !! pos2)
    in take pos3 code ++ [a * b] ++ drop (pos3 + 1) code

testinputs :: [Int] -> Int -> [(Int, Int)]
testinputs code target =
    [(x, y) | x <- [0..99], y <- [0..99], intcode (1:x:y:drop 3 code) 0 == target]