oddEven :: [a] -> [a]
oddEven [] = []
oddEven(x:y:xs) = (y) : (x) : (oddEven xs)
oddEven(x:xs) = (x) : (oddEven xs)

insert :: [a] -> a -> Int -> [a]
insert list atom pos = (take pos list) ++ [atom] ++ (drop pos list)

listSumm :: [Int] -> [Int] -> [Int]
listSumm [] _ = []
listSumm _ [] = []
listSumm (x:xs) (y:ys) = (x + y) : (listSumm xs ys)

position :: Eq a => [a] -> a -> Int
position [] _ = 0
position (x:xs) atom 
    | (x /= atom) = 1 + position xs atom
    | otherwise = 0

getSum :: Int -> Int
getSum n = sum [1..n]

getSum1 :: Int -> Int
getSum1 n = getSum n - n

--main = do
--    print (oddEven ['u', 'l', 'k', 'c', 'y'])
--    print (insert [1,2,3,4,5] 2 7)
--    print (listSumm [1,2,3,4] [2,3,4,5])
--    print (position [1,2,3,4] 3)
--    print (getSum 5)
--    print (getSum1 5)
