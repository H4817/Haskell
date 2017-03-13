import System.IO
import System.Environment   


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

