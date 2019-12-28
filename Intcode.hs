module Intcode ( Input,
                 Opcode,
                 Offset,
                 RelBase,
                 Code (Program, Output, Pause, Done),
                 Mode,
                 Parameter,
                 Operation,
                 Computer (Computer),
                 runCode
                 ) where

import Data.List

type Opcode = Int
type Input = Int
type Offset = Int
type RelBase = Int

data Code   = Program [Int] | Output Int | Pause [Int] | Done deriving (Show, Eq)
programmed (Pause x) = Program x

data Mode = Position | Immediate | Relative deriving (Show, Eq)
modeFromChar :: Char -> Mode
modeFromChar s = case s of '0' -> Position
                           '1' -> Immediate
                           '2' -> Relative

data Parameter = Parameter { value :: Int
                           , mode  :: Mode
                           , relBase  :: RelBase
                           } deriving (Show)
parameterFromTuple :: (Int, Mode, RelBase) -> Parameter
parameterFromTuple (value, mode, base) = Parameter value mode base

data Operation = Operation { opcode     :: Opcode
                           , parameters :: [Parameter]
                           } deriving (Show)

data Computer = Computer { code    :: Code
                         , offset  :: Offset
                         , base    :: RelBase
                         , input   :: Input
                         } deriving (Show)
offsetComputer :: Computer -> Int -> Computer
offsetComputer (Computer code offset base input) noffset = Computer code noffset base input

operationFromCode :: Code -> RelBase -> Operation
operationFromCode (Program (op:params)) base
    | not $ null params = Operation (read op') params'
    | otherwise         = Operation (read op') []
    where (modes, op')  = splitAt 3 $ fiveDigit op
          params'       = zipWith3 curried params (map modeFromChar (reverse modes)) (repeat base)
          curried a b c = parameterFromTuple (a, b, c)

fiveDigit :: Int -> String
fiveDigit x = reverse $ take 5 $ reverse (show x) ++ repeat '0'

listFromInt :: Int -> [Int]
listFromInt x = map (\y -> read [y]) $ show x

operate :: (Int -> Int -> Int) -> [Parameter] -> Code -> Code
operate f [x, y, p] (Program code) = let x' = output [x] (Program code)
                                         y' = output [y] (Program code)
                                         p' = if mode p == Relative then value p + relBase p else value p
                                     in Program $ take p' code ++ [f x' y'] ++ drop (p' + 1) code
add = operate (+)
mult = operate (*)

compareParam :: (Int -> Int -> Bool) -> [Parameter] -> Code -> Bool
compareParam cond [x, y] code = let x' = output [x] code
                                    y' = output [y] code
                                    in cond x' y'
isEqual = compareParam (==)
isLessThan = compareParam (<)

isZero :: [Parameter] -> Code -> Bool
isZero [x] = isEqual [x, parameterFromTuple (0, Immediate, 0)]

input' :: Input -> [Parameter] -> Code -> Code
input' i (p:_) (Program code)
    | mode p == Relative = Pause $ take (value p + relBase p) code ++ [i] ++ drop (value p + relBase p + 1) code 
    | otherwise          = Pause $ take (value p) code ++ [i] ++ drop (value p + 1) code 

output :: [Parameter] -> Code -> Int
output (p:_) (Program code) = case mode p of Immediate -> value p
                                             Position  -> code !! value p
                                             Relative  -> code !! (value p + relBase p)

adjustBase :: [Parameter] -> Code -> RelBase
adjustBase (p:_) code = relBase p + output [p] code

execute :: Input -> Offset -> Operation -> Code -> (Code, Offset, RelBase)
execute i offset op code
    | op' == 1 = (add params code, offset + 4, base)
    | op' == 2 = (mult params code, offset + 4, base)
    | op' == 3 = (input' i params code, offset + 2, base)
    | op' == 4 = (Output (output params code), offset + 2, base)
    | op' == 5 = (code, if isZero [p1] code then offset + 3 else get p2, base)
    | op' == 6 = (code, if isZero [p1] code then get p2 else offset + 3, base)
    | op' == 7 = (if isLessThan [p1, p2] code then programmed $ code' 1 else programmed $ code' 0, offset + 4, base)
    | op' == 8 = (if isEqual [p1, p2] code then programmed $ code' 1 else programmed $ code' 0, offset + 4, base)
    | op' == 9 = (code, offset + 2, adjustBase params code)
    | op' == 99 = (Done, -1, base)
    | otherwise = error $ "Unsupported opcode " ++ show op'
    where (op', params) = (opcode op, parameters op)
          (p1:p2:p3) = params
          base    = relBase $ head params
          get x   = output [x] code
          code' x = input' x p3 code

runCode :: Computer -> (Code, Computer)
runCode (Computer (Program code) offset base input) = (code', Computer code' offset' base' input)
    where o = read $ drop 3 (fiveDigit (code !! offset))
          o'
            | o <= 2 = 4
            | o <= 4 = 2
            | o <= 6 = 3
            | o <= 8 = 4
            | o == 9 = 2
            | otherwise = 1
          operation = operationFromCode (Program (take o' $ drop offset code)) base
          (code', offset', base') = execute input offset operation $ Program code
runCode (Computer (Pause code) offset base input) = (Program code, Computer (Program code) offset base input)
runCode comp@(Computer code _ _ _) = (code, comp)