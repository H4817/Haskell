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


main = do
    print (insert' 4 [3,5,1,2,8,2])
    print (partition' (<5) [1..12])
    print (lines' "lolxd\ntestXd\n")
