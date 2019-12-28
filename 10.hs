import Data.List
import qualified Data.Map as M

type Point = (Rational, Rational)
type LinesBetween = (Point, [(Line, Point)])
type SightLines = M.Map Line [Point]

data Line = Vertical Rational | Diagonal Rational Rational deriving (Show, Eq, Ord)
lineFromPoints :: Point -> Point -> Line
lineFromPoints (x1, y1) (x2, y2)
    | x1 == x2  = Vertical x1
    | otherwise = Diagonal slope intersect
    where slope     = (y2 - y1) / (x2 - x1)
          intersect = y1 - (x1 * slope)

perpendicular :: Line -> Point -> Line
perpendicular (Vertical _) (_, y) = Diagonal 0 y
perpendicular (Diagonal s i) (x, y)= Diagonal (-1/s) (y - (-1/s)) 

isElem :: Point -> Line -> Bool
isElem p l = ordPoint p l == EQ

angle :: Line -> Double
angle (Vertical _) = pi / 2
angle (Diagonal slope _) = let x = atan $ fromRational slope in if x >= 0 then x else pi + x

angleFromPoints :: Point -> Point -> Double
angleFromPoints p1@(x1, y1) p2@(x2, y2) = let a = angle (lineFromPoints p1 p2) in if y1 < y2 then a else a + pi

angleBetween :: Point -> Point -> Point -> Double
angleBetween (x1', y1') (x2', y2') (x3', y3') = let [x1,y1,x2,y2,x3,y3] = map fromRational [x1',y1',x2',y2',x3',y3']
                                                in atan2 (y2 - y1) (x2 - x1) - atan2 (y3 - y1) (x3 - x1)

ordPoint :: Point -> Line -> Ordering
ordPoint (x, y) (Vertical a) = y `compare` a
ordPoint (x, y) (Diagonal slope intersect) = y `compare` (x * slope + intersect)

idenLine :: Point -> Line
idenLine (x, y) = Diagonal 1 (y - x)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ rise^2 + run^2
    where rise = fromRational y2 - fromRational y1
          run  = fromRational x2 - fromRational x1

asteroidMap :: String -> Rational -> [Point]
asteroidMap [] _ = []
asteroidMap str i = filter (/= (-1, -1)) $ line ++ asteroidMap (unlines xs) (i + 1)
    where (x:xs) = lines str
          line   = zipWith3 f x (take (length x) [0..]) (repeat i)
          f a b c = if a == '#' then (b, c) else (-1, -1)

linesMap :: [Point] -> [LinesBetween]
linesMap points = [(a, sort [(lineFromPoints a b, b) | b <- points, a /= b]) | a <- points]

sightLines :: LinesBetween -> SightLines
sightLines (p@(x, y), lines) = M.fromListWith (++) (map (fmap (:[])) $ concat $ concat groupedSplit)
    where grouped = groupByFirst lines
          groupedSplit = map (groupByIdenOrd p) grouped

-- sightLines' :: LinesBetween -> SightLines
sightLines' (p@(x, y), lines) = (p, concat groupedSplit)
    where grouped = groupByFirst lines
          groupedSplit = map (groupByIdenOrd p) grouped

groupByFirst :: Eq a => [(a, b)] -> [[(a, b)]]
groupByFirst = groupBy (\(a, _) (b, _) -> a == b)

idenOrd :: (Line, Point) -> Point -> Ordering
idenOrd (l, a) p@(x, _) = if l == idenLine p then ordPoint a (Vertical x) else ordPoint a (idenLine p)

groupByIdenOrd :: Point -> [(Line, Point)] -> [[(Line, Point)]]
groupByIdenOrd p = groupBy (\a b -> idenOrd a p == idenOrd b p)

laser :: Point -> Point -> SightLines -> (Point, SightLines)
laser o p l = (target, l')
    where (a, b) = M.partition (elem p) l 
          [(k, ps)] = M.assocs a
          compf x y = (angleBetween o x p `compare` angleBetween o y p) `mappend`
                      (distance o x `compare` distance o y)
          sorted = sortBy compf (concat $ M.elems b)
          target = head $ filter (\ x -> 0 > angleBetween o p x) sorted ++ sorted
          l' = M.insert k (delete p ps) l

laserLoop :: Int -> Int -> Point -> Point -> SightLines -> Point
laserLoop i t o p l = if i == t then p else let (a, b) = laser o p l in laserLoop (i+1) t o a b

laserLoop' :: Point -> Point -> SightLines -> [Point]
laserLoop' o p l = let (a, b) = laser o p l in p : laserLoop' o a b

main = do
    c <- readFile "10.txt"
    let m = linesMap (asteroidMap c 0)
        a = M.fromList m
        b = a M.! (14, 17)
        d = sightLines ((14, 17), b)
        r = last $ take 200 $ laserLoop' (14, 17) (14, 16) d
        
    print r