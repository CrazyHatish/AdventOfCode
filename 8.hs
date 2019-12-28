import Data.List
import Data.Char

type Pixel = Int
type Row   = [Pixel]
type Layer = [Row]
type Image = [Layer]

layerFromText :: (Int, Int) -> String -> Layer
layerFromText _ [] = []
layerFromText (w, h) str = let (a, b) = splitAt w str in map (read . (:[])) a: layerFromText (w, h) b

imageFromText :: (Int, Int) -> String -> Image
imageFromText _ [] = []
imageFromText (w, h) str = let (a, b) = splitAt (w*h) str in layerFromText (w, h) a:imageFromText (w, h) b

countElements :: (Eq a) => a -> [[a]] -> Int
countElements x = sum . map (length . filter (x ==))

fewestZero :: Image -> Layer
fewestZero = minimumBy (compareWith (countElements 0))
    where compareWith f a b = compare (f a) (f b)

puzzleSolution :: Layer -> Int
puzzleSolution layer = countElements 1 layer * countElements 2 layer

puzzleSolution' :: Image -> (Int, Int) -> Layer
puzzleSolution' img (w, _) = splitImg $ foldl1 (zipWith (\x y -> if x == 2 then y else x)) $ map concat img
    where splitImg [] = []
          splitImg pixels = let (a, b) = splitAt w pixels in a:splitImg b

main = do
    contents <- readFile "8.txt"
    let img = imageFromText (25, 6) contents
        s = puzzleSolution' img (25, 6)
        s' = map (map intToDigit)
    print $ s' s