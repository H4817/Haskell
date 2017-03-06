
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | (all (/=x) xs) = x : removeDuplicates xs
    | otherwise = removeDuplicates xs


insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs) 
    | (x >= a) = a : x : xs
    | otherwise = x : insert' a xs


union' :: Eq a => [a] -> [a] -> [a]
union' [] _ = []
union' _ [] = []
union' list1 list2 = removeDuplicates (list1 ++ list2)


intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) list
    | ((any (x==) list)) = x : intersect' xs list
    | otherwise = intersect' xs list


partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' p list = (filter p list, filter (not . p) list)


lines' :: String -> [ String ]
lines' [] = []
lines' (x:xs)
    | (x == '\n') = (lines' xs)
    | otherwise = [x] : lines' xs

position :: Eq a => [a] -> a -> Int
position [] _ = 0
position (x:xs) atom 
    | (x /= atom) = 1 + position xs atom
    | otherwise = 0


elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' atom (x:xs) 
    | (x /= atom) = elemIndices' atom xs
    | otherwise = (elemIndices' atom (drop (position xs atom) xs)) ++ []



main = do
    print (insert' 4 [3,5,1,2,8,2])
    print (partition' (<5) [1..12])
    print (lines' "lolxd\ntestXd\n")
    print (elemIndices' 4 [1,2,3,4,4,5,6,3,4])
