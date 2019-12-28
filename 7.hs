import Intcode
import Data.List
import qualified Data.Map.Strict as Map

data Amplifier = A | B | C | D | E deriving (Show, Eq, Ord, Enum)

type Memory  = Map.Map Amplifier (Code, Phase, Bool, Offset, RelBase)
type Phase   = Int
type Offset  = Int
type RelBase = Int

inputList = [9,7,8,5,6] :: [Int]
program   = Program [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,109,126,207,288,369,450,99999,3,9,102,4,9,9,1001,9,4,9,102,2,9,9,101,2,9,9,4,9,99,3,9,1001,9,5,9,1002,9,5,9,1001,9,5,9,1002,9,5,9,101,5,9,9,4,9,99,3,9,101,5,9,9,1002,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,102,4,9,9,101,2,9,9,102,4,9,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,101,5,9,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,99]

mem = Map.fromList $ zip (enumFrom A) $ zip5 (repeat program) inputList (repeat True) (repeat 0) (repeat 0) :: Memory

runAmps :: [Input] -> (Code, Int, Code) -> Int
runAmps input (code, offset, c)
    | t == 4 = let (Output n) = code' in runAmps (x:n:xs) (code, offset', c)
    | t == 99 = if x == minBound then head xs else runAmps (x:xs) (c, 0, c)
    | t == 3 = runAmps xs (code', offset', c)
    | otherwise = runAmps (x:xs) (code', offset', c)
    where (code', offset', t, _) = runCode x offset 0 code
          (x:xs) = if not $ null input then input else [minBound]

runAmps' :: Input -> Memory -> Amplifier -> Int
runAmps' input mem amp
    | t == 4 = let (Output n) = code' in runAmps' n mem'' amp'
    | t == 99 = input
    | otherwise = runAmps' input mem' amp
    where (code, phase, cond, offset, base) = mem Map.! amp
          cond'  = t /= 3 && cond
          mem'   = Map.insert amp (code', phase, cond', offset', base') mem
          mem''  = Map.insert amp (code, phase, cond', offset', base') mem
          amp'   = if amp == E then A else succ amp
          input' = if cond then phase else input
          (code', offset', base', t) = runCode input' offset base code


maxValue :: Code -> [Int] -> Int
maxValue code possible = maximum $ map (\(x:xs) -> runAmps (x:0:xs) (code, 0, code)) $ permutations possible

maxValue' :: Code -> [Int] -> Int
maxValue' code possible = maximum $ map (\x -> runAmps' 0 (mem x) A) $ permutations possible
    where mem a = Map.fromList $ zip (enumFrom A) $ zip5 (repeat program) a (repeat True) (repeat 0) (repeat 0)

