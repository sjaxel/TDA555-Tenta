import Data.List
import Data.Maybe
import Data.Char
import System.Directory
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


-- 2011-08 Fråga 6 "Wordsnake" feat. Lillen

type Snake = [String]

s1 :: [String]
s1 = ["ahoy", "hola", "okay", "yahoo", "obrigado", "haskell"]

s2 :: Snake
s2 = ["hola","ahoy","yahoo","obrigado","okay"]

snake :: [String] -> Snake
snake list = longest [snake' x list | x<-list]

longest :: [Snake] -> Snake
longest [] = []
longest (x:xs)
	| (length x) >= (length (longest xs)) = x
	| otherwise = longest xs

snake' :: String -> [String] -> Snake
snake' ord list = (ord) : (longest (map (\y-> snake' y xs') [y | y<-xs', (head y) == lastL]))
	where lastL = last ord
	      xs' = delete ord list

-- 2012-08 "Skriv filer i filsystem"

data File
    = File Name
    | Dir Name [File]

type Name = String

type FileSystem = [File]

fs :: FileSystem
fs =
  [ Dir "English"
      [ Dir "Beatles"
          [ File "she_loves_you.mp3" ]
      , Dir "Supertramp"
          [ File "album_art.jpeg"
          , File "crazy.mp3"
          , File "school.mp3" ]
      ]
  , Dir "Swedish"
      [ File "du_gamla_du_fria.mp3"
      , Dir "Kent"
          [ File "album_art.jpeg"
          , File "ingenting.mp3" ]
          , File "tontarna.mp3"
      ]
  ]

imprint :: FileSystem -> IO ()
imprint fs = mapM_ (imprintF) fs

imprintF :: File -> IO ()
imprintF (File file)   = do writeFile file ""
imprintF (Dir name fs) = do
    createDirectory name
    currentDir <- getCurrentDirectory
    setCurrentDirectory name
    imprint fs
    setCurrentDirectory currentDir

-- 2011-01 "Ta bort taggar"



annasSida :: Doc
annasSida =
  [ Text "Welcome to my website!"
  , Tag "P" [ Tag "B" [ Text "My hobbies are "
                      , Tag "EM" [ Text "Haskell" ]
                      , Text " programming and playing "
                      , Tag "EM" [ Text "Myst" ]
                      , Text "."
                      ] ]
  , Tag "P" [ Text "Thanks for visiting! "
            , Tag "EM" [ Text "anna@gmail.com" ]
            ]
  ]

removeTag :: String -> Doc -> String
removeTag s  doc = unwords (filter (/=) (checkHTML' doc))

checkHTML' :: Doc -> [String]
checkHTML' doc = concatMap readHTML' doc

readHTML' :: DocPart -> [String]
readHTML' (Text s) = words s
readHTML' (Tag s doc) = [("<" ++ s ++">")] ++ (checkHTML' doc) 
                        ++ [("</" ++ s ++">")] 






















