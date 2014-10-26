import Data.List
import Data.Maybe
import Data.Char
import Test.QuickCheck

-- 2012-08 Fråga 6
example :: [String]
example = words "The cat (Felis catus), also known as the domestic cat or housecat to distinguish it from other felids and felines, is a small, usually furry, domesticated, carnivorous mammal that is valued by humans for its companionship and for its ability to hunt vermin and household pests."

--      width   words       lines
fill :: Int -> [String] -> [String]
fill w []    = []
fill w words = [unwords a] ++ (fill w b)
    where (a, b) = split w ([], words)

split :: Int -> ([String], [String]) -> ([String], [String])
split _ (a, [])          = (a, [])
split w ([], (b:bs))     = split w (([b]), (bs))
split w (a, (b:bs))      | length (unwords (a ++ [b])) > w
                         = (a, (b:bs))
                         | otherwise = split w ((a ++ [b]), (bs))

-- 2014-01 Fråga 7

data DocPart
  = Text String
  | Tag String Doc

type Doc = [DocPart]

importantHTML :: Doc -> [String]
importantHTML doc = concatMap readHTML doc

readHTML :: DocPart -> [String]
readHTML (Text _)     = []
readHTML (Tag "EM" d) = concatMap saveHTML d
readHTML (Tag _ d)    = concatMap readHTML d

saveHTML :: DocPart -> [String]
saveHTML (Text s)     = words s
saveHTML (Tag _ d)    = concatMap saveHTML d

-- 2014-01 Fråga 6

data Command
  = FORW Int
  | BACKW Int
  | LEFT
  | RIGHT
  deriving (Show, Eq)
  
destination :: [Command] -> (Int,Int)
destination cmd = drive (replicate (mod (turns cmd) 4) LEFT) 
                  $ drive cmd (0,0)

drive :: [Command] -> (Int, Int) -> (Int, Int)
drive []             (x, y) = (x, y)
drive ((FORW d):cs)  (x, y) = drive cs (x, (y+d))
drive ((BACKW d):cs) (x, y) = drive cs (x, (y-d))
drive ((LEFT):cs)    (x, y) = drive cs (y, (-x))
drive ((RIGHT):cs)   (x, y) = drive cs ((-y), x)

turns :: [Command] -> Int
turns cmd = ((length.filter (==RIGHT)) cmd) - ((length.filter (==LEFT)) cmd)

-- 2013-10 Fråga 7 "Max spend"

maxSpend :: Int -> [Int] -> Int
maxSpend i l = maximum [sum x | x <- (subSets l), sum x <= i]

subSets :: [Int] -> [[Int]]
subSets []     = [[]]
subSets (x:xs) = subSets xs ++ map (x:) (subSets xs)







