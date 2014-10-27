import Data.List
import Data.Maybe
import Data.Char
import System.Directory
import Test.QuickCheck

-- 2014-01 --

-- 1)

findI :: Eq a => a -> [a] -> Int
findI a xs = length (takeWhile (/=a) xs)

-- 2)

extension :: String -> String
extension str = '.':(reverse (takeWhile (/='.') (reverse str)))

-- 3)

data Store
  = Empty
  | Join Int Store Store

size :: Store -> Int
size (Empty) = 0
size (Join _ s1 s2) = 1 + (size s1) + (size s2)

-- 4)

prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append xs ys =
  length (xs ++ ys) == length (xs) + length (ys)

-- 5)

initFile :: IO ()
initFile = do
    putStr "Filnamn "
    fn <- getLine
    putStr "Innehåll "
    fc <- getLine 
    writeFile fn fc
