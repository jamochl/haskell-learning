-- file: ch02/myDrop.hs
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne xs = if (length xs > 2)
                then lastButOne (tail xs)
                else head xs
