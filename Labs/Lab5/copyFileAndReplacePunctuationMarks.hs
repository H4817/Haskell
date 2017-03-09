import System.IO
import Data.Char ( toUpper )
import System.Environment   


copyFile h1 h2 = do
    eof <- hIsEOF h1
    if eof then return () else
        do
            c <- hGetChar h1
            hPutChar h2 (toUpper c)   
            copyFile h1 h2


replacePunctuationMarks :: Char -> Char -> Char
replacePunctuationMarks replaceBy inputChar
    | (any (inputChar==) [',',':',';','"','!','?','#','@','-','+','\'']) = replaceBy
    | otherwise = inputChar


copyFileAndReplacePunctuationMarks :: String -> String -> IO ()
copyFileAndReplacePunctuationMarks f1 f2 = putStrLn "Hello, enter symbol for replace punctuation marks: " >>
           getChar >>= \symbol ->
           putChar symbol



main = do
         putStrLn "Hello, enter symbol for replace punctuation marks: "
         replaceBy <- getChar
         s <- readFile "in.txt"
         writeFile "out.txt" (map (replacePunctuationMarks replaceBy) s)

