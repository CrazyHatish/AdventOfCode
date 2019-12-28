import Data.List
import Data.Char

range = (372304, 847060) :: (Int, Int)

adjacent :: [Int] -> Bool
adjacent [_]      = False
adjacent (x:y:xs) = x == y || adjacent (y:xs)

adjacent' :: [Int] -> Bool
adjacent' xs = 2 `elem` map length (group xs)

listFromInt :: Int -> [Int]
listFromInt x = map (\y -> read [y]) $ show x

testConditions :: Int -> Bool
testConditions x = let xlist = listFromInt x in adjacent xlist && sort xlist == xlist

testConditions' :: Int -> Bool
testConditions' x = let xlist = listFromInt x in adjacent' xlist && sort xlist == xlist