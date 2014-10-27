import Data.List
import Data.Maybe
import Data.Char
import Test.QuickCheck

-- 2012-08 Fråga 6 "Radlängd"
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

-- 2014-01 Fråga 7 "ImportantHTML"

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

-- 2014-01 Fråga 6 "Radiobilen"
c1 :: [Command]
c1 = [FORW (-20), BACKW 10, RIGHT, FORW 100]

data Command
  = FORW Int
  | BACKW Int
  | LEFT
  | RIGHT
  deriving (Show, Eq)
  
destination :: [Command] -> (Int,Int)
destination cmd = foldl drive (0, 0) (cmd ++ (replicate (turns cmd) LEFT))

drive ::  (Int, Int) -> Command -> (Int, Int)
drive (x, y) (FORW d)   = (x, (y+d))
drive (x, y) (BACKW d)  = (x, (y-d))
drive (x, y) (LEFT)     = (y, (-x))
drive (x, y) (RIGHT)    = ((-y), x)

turns :: [Command] -> Int
turns cmd = mod (((length.filter (==RIGHT)) cmd) - ((length.filter (==LEFT)) cmd)) 4

-- 2013-10 Fråga 7 "Max spend"

maxSpend :: Int -> [Int] -> Int
maxSpend i l = maximum [sum x | x <- (subSets l), sum x <= i]

subSets :: [Int] -> [[Int]]
subSets []     = [[]]
subSets (x:xs) = subSets xs ++ map (x:) (subSets xs)


-- 2011-08 Fråga 6 "Wordsnake"

type Snake = [String]

s1 :: [String]
s1 = ["ahoy", "hola", "okay", "yahoo", "obrigado", "haskell"]

s2 :: Snake
s2 = ["hola","ahoy","yahoo","obrigado","okay"]

--snake :: [String] -> Snake
snake list = [ x | x <- (perms list), (isSnake x) == True]

isSnake :: [String] -> Bool
isSnake (x1:x2:[]) = and [last x1 == head x2]
isSnake (x1:x2:xs) = and ([last x1 == head x2] ++ [isSnake (x2:xs)])

perms [] = [[]]
perms xs = [ x:ps | x <- xs, ps <- perms (xs\\[x]) ]















