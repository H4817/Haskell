mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst _ [] = []
mysubst (x:xs) list
    | ((all (x/=) list)) = x : mysubst xs list
    | otherwise = mysubst xs list


getStringTillEol :: String -> String
getStringTillEol [] = []
getStringTillEol (x:xs)
    | (x /= '\n') = x : getStringTillEol xs
    | otherwise = []


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | (all (/=x) xs) = x : removeDuplicates xs
    | otherwise = removeDuplicates xs





-- will start at the beginning of the list and then keep going until it finds an element that's equal to
-- or greater than the element that we're inserting and it will insert it just before the element.
insert' :: Ord a => a -> [a] -> [a] 
insert' a [] = [a]
insert' a (x:xs) 
    | (x >= a) = a : x : xs
    | otherwise = x : insert' a xs


-- concatenate two lists then remove duplicates
union' :: Eq a => [a] -> [a] -> [a]
union' [] _ = []
union' _ [] = []
union' list1 list2 = removeDuplicates (list1 ++ list2)


-- it returns only the elements that are found in both lists.
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) list
    | ((any (x==) list)) = x : intersect' xs list
    | otherwise = intersect' xs list


-- takes a list and a predicate and returns a pair of lists. The first list in the result contains
-- all the elements that satisfy the predicate, the second contains all the ones that don't.
partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' p list = (filter p list, filter (not . p) list)


-- it takes a string and returns every line of that string in a separate list.
lines' :: String -> [String] 
lines' [] = []
lines' (x:xs) = getStringTillEol (x:xs) : lines' (tail(mysubst xs (getStringTillEol xs)))


-- main = do
--     putStrLn "-----insert-----"
--     print (insert' 4 [3,5,1,2,8,2])
--     print (insert' 4 [1,3,4,4,1])
--     print (insert' 3 [1,2,4,3,2,1])
--     putStrLn "-----union-----"
--     print ([1..7] `union'` [5..10])
--     print ([5..12] `union'` [2..8])
--     print ([1..3] `union'` [2..6])
--     putStrLn "-----intersect-----"
--     print ([1..7] `intersect'` [5..10])
--     print ([5..12] `intersect'` [2..8])
--     print ([1..3] `intersect'` [2..6])
--     putStrLn "-----partition-----"
--     print (partition' (>3) [1,3,5,6,3,2,1,0,3,7])
--     print (partition' (odd) [1..11])
--     print (partition' (<2) [2..6])
--     putStrLn "-----lines-----"
--     print (lines' "hello\nagain\n")
--     print (lines' "word\ntestx\n")
--     print (lines' "good\nluck\n")
