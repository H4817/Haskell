getMultiplicityElements :: Int -> Int -> Int -> [Int]
getMultiplicityElements startWith amount multiplicity = [ x | x <- [startWith..amount*multiplicity], ((x `mod` multiplicity) == 0) ]

-- main = do
--     print (getMultiplicityElements 4 7 241)
