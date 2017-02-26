listnums :: Int -> [Int]
listnums n
    | (n > 0) = (n) : (listnums (n-1))
    | otherwise = []


secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist (x:xs) = (last x) : (secondlastlist xs)


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | (all (/=x) xs) = x : removeDuplicates xs
    | otherwise = removeDuplicates xs


myunion :: Eq a => [a] -> [a] -> [a]
myunion [] _ = []
myunion _ [] = []
myunion list1 list2 = removeDuplicates (list1 ++ list2)


mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst _ [] = []
mysubst (x:xs) list
    | ((all (x/=) list)) = x : mysubst xs list
    | otherwise = mysubst xs list


getnElements :: [[a]] -> Int -> [a]
getnElements list pos = map (!!pos) list


--main = do
--    print (listnums 15)
--    print (secondlastlist [[1,2,3,4], [2,5,7], [11,23,33]])
--    print (getnElements [[2,2,3,4], [2,5,7], [11,23,33]] 1)
--    print (myunion ['w', 'o', 'r', 'd'] ['w', 'a', 'r', 'd'])
--    print (mysubst [1,2,3,4] [3,4,5,6,7])
