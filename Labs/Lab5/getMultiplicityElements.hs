getMultiplicityElements :: Int -> Int -> Int -> [Int]
getMultiplicityElements startWith amount multiplicity = [ x | x <- [startWith..amount*multiplicity], ((x `mod` multiplicity) == 0) ]

main = do
    putStrLn "Hello input a starting point amount and multiplicity: "
    startWith <- getLine
    amount <- getLine
    multiplicity <- getLine
    print (getMultiplicityElements (read startWith) (read amount) (read multiplicity))
