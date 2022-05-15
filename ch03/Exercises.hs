-- file ch03/Exercises.hs 
import Data.List

-- 1. and 2.
fakeLength :: [a] -> Int
fakeLength [] = 0
fakeLength (x:xs) = 1 + fakeLength xs

-- 3.
meanList :: Fractional a => [a] -> a
meanList [] = 0
meanList x = (sum x) / fromIntegral (length x)

-- 4.
palindrome :: [a] -> [a]
palindrome [] = []
palindrome x = x ++ (reverse x)

-- 5.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome x = x == (reverse x)

-- 6.
sortLenList :: [[a]] -> [[a]]
sortLenList [] = []
sortLenList x = sortBy listLen x
    where listLen a b = compare (length a) (length b)

testList = [[1,2,3],[1,2],[1]]

-- 7.
fakeIntersperse :: Char -> [[Char]] -> [Char]
fakeIntersperse _ [] = ""
fakeIntersperse x (y:[]) = y
fakeIntersperse x (y:ys) = y ++ [x] ++ (fakeIntersperse x ys)

fakeIntersperseTwo :: Char -> [[Char]] -> [Char]
fakeIntersperseTwo _ [] = ""
fakeIntersperseTwo _ (y:[]) = y
fakeIntersperseTwo x (y:ys) = foldl (\a b -> a ++ [x] ++ b) y ys

-- 8.
-- file: ch03/ListADT.hs
data Tree a = Tree a (Tree a) (Tree a)
                | Empty
                  deriving (Show)

realTree = Tree 1
           (Tree 2 (Empty) (Empty))
           (Tree 3
                 (Tree 4
                        (Tree 6
                               (Empty) (Empty))
                        (Empty))
                 (Tree 5
                        (Empty)
                        (Tree 7
                               (Tree 8
                                       (Empty)
                                       (Empty))
                               (Empty))))

-- My attempt
treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Tree _ Empty Empty) = 1
treeHeight (Tree _ Empty b) = 1 + treeHeight b
treeHeight (Tree _ a Empty) = 1 + treeHeight a
treeHeight (Tree _ a b) = let deepA = 1 + treeHeight a
                              deepB = 1 + treeHeight b
                            in if deepA > deepB
                               then deepA
                               else deepB

-- user Ivan (sooo much better than mine)
treeHeightBetter :: Tree a -> Int
treeHeightBetter Empty = 0
treeHeightBetter (Tree x left right) = 1 + max (treeHeightBetter left) (treeHeightBetter right)

-- 9. 
type Coordinate = (Integer, Integer)
data Direction = LeftAngle
               | RightAngle
               | StraightAngle
                 deriving (Show,Eq)

-- 10. This is a ridiculous coding exercise - It's all maths!
calcDirection :: Coordinate -> Coordinate -> Coordinate -> Direction
-- calcDirection a b c
calcDirection (ax,ay) (bx,by) (cx,cy)
  | resultOfK < 0 = RightAngle
  | resultOfK > 0 = LeftAngle
  | otherwise = StraightAngle
  where resultOfK = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  -- where vector (ax,ay) (bx,by) = (bx - ax, by - ay)
  --       vecOne = vector a b
  --       vecTwo = vector a c
  --       crossProdOfK (ax, ay) (bx, by) = ax * by - ay * bx
  --       resultOfK = crossProdOfK vecOne vecTwo

coorA = (0,0)
coorB = (2,4)
coorC = (2,2)

-- 11.
-- My attempt
calcTripleDirection :: [Coordinate] -> [Direction]
calcTripleDirection [] = []
calcTripleDirection x
  | length x >= 3      = (calcTriplet (take 3 x)) : calcTripleDirection (tail x)
  | otherwise          = []
  where calcTriplet (a:b:c:[]) = calcDirection a b c

-- User gmaths
successivePoints :: [Coordinate] -> [Direction]
successivePoints (x:y:z:xs) = (calcDirection x y z) : successivePoints (y:z:xs)
successivePoints _ = []

-- 12.
-- What is wanted
exampleHull = [(0,0), (4,4), (4,1), (2,3), (0,4)]
-- after the graham scan, this should be
-- [(0,0), (4,1), (4,4), (0,4)]

-- takes two lists of coordinates, first is points, second is stack
-- FIXED BUG, forgot to sort list before applying algo
grahamScan :: [Coordinate] -> [Coordinate] -> [Coordinate]
grahamScan [] [] = []
grahamScan (p:[]) [] = p:[]
grahamScan p [] = grahamScan ps (p2:p1:[])
    where (p1:p2:ps) = grahamSort p
grahamScan (p:ps) (s:[]) = grahamScan ps (p:s:[])
grahamScan [] s = reverse s
grahamScan (p:ps) (s1:s2:ss)
  | d == RightAngle || d == StraightAngle  = grahamScan ps (p:s1:s2:ss)
  | otherwise                             = grahamScan (p:ps) (s2:ss)
  where d = calcDirection s1 s2 p

grahamSort [] = []
grahamSort x = (xp0, yp0) : sortOn condition xs
    where ((xp0, yp0):xs) = sortOn snd (sortOn fst x)
          condition (x2,y2) = atan((fromIntegral (y2 - yp0)) / (fromIntegral (x2 - xp0)))
