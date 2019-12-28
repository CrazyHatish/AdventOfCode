import Data.List
import qualified Data.Map.Strict as Map  

type Id = String
type Orbits = Map.Map Id [Id]
type Path = [Id]

data Object = Object {idd :: Id, orbits :: [Object]} deriving (Show, Eq)

orbitInsert :: Orbits -> String -> Orbits
orbitInsert orbs s = let (k, (_:v)) = span (/= ')') s in Map.insertWith (++) k [v] orbs

orbitMap :: Orbits -> Id -> Object
orbitMap orbs id
    | orbs' == [] = Object id []
    | otherwise   = Object id $ map (orbitMap orbs) orbs'
    where orbs' = Map.findWithDefault [] id orbs

orbitsCount :: Int -> Object -> Int
orbitsCount steps (Object _ [])   = steps
orbitsCount steps (Object _ orbs) = steps + (sum $ map (orbitsCount $ steps+1) orbs)

findPath :: Id -> Object -> Path
findPath id o
    | id' == id = ["Complete"] 
    | o' == [] && id /= id' = []
    | otherwise = foldr (\x acc -> if path x /= [] then (idd x):(path x) else acc) [] o'
    where Object id' o' = o
          path = findPath id

distance :: Id -> Id -> Object -> Int
distance x y o = -1 + (length $ reverse $ strip lx ++ (strip ly))
    where (px, py) = ((findPath x o) ++ repeat "", (findPath y o) ++ repeat "")
          (lx, ly) = unzip $ dropWhile (\a -> fst a == snd a) $ zip px py
          strip = takeWhile (/= "Complete")

main = do
    contents <- readFile "6.txt"
    let l = lines contents
    let orbs = foldl orbitInsert Map.empty l 
        orbMap = orbitMap orbs "COM"
        count = orbitsCount 0 orbMap
        p = findPath "YOU" orbMap
        s = findPath "SAN" orbMap
        d = distance "YOU" "SAN" orbMap - 1
    print d