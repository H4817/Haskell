import System.IO
import System.Environment   


copyFile h1 h2 = do
    eof <- hIsEOF h1
    if eof then return () else
        do
            c <- hGetChar h1
            hPutChar h2 (c)   
            copyFile h1 h2


replacePunctuationMarks :: Char -> Char -> Char
replacePunctuationMarks replaceBy inputChar
    | (any (inputChar==) [',',';',':','.','!','?','`','\'','"','-','—','/','(',')','[',']','*']) = replaceBy
    | otherwise = inputChar


copyFileAndReplacePunctuationMarks :: String -> String -> IO ()
copyFileAndReplacePunctuationMarks inputFile outputFile = 
    do
        putStrLn "Hello, enter symbol for replace punctuation marks: "
        replaceBy <- getChar
        s <- readFile inputFile
        writeFile outputFile (map (replacePunctuationMarks replaceBy) s)



main = do
           [inp, out] <- getArgs 
           copyFileAndReplacePunctuationMarks inp out

