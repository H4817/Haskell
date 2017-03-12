import System.IO
import qualified Data.ByteString as Str
import Data.Char

-- get content +-
-- append 
-- delete concrete line
-- copy file with filter

-- getFileContent :: String -> IO ()
-- getFileContent fileName = do
--     file <- openFile fileName ReadMode
--     content <- hGetContents file
--     print content
    -- hClose file

add :: String -> String -> IO ()  
add fileName todoItem = appendFile fileName (todoItem ++ "\n")  


view :: [String] -> IO ()  
view [fileName] = do  
        contents <- readFile fileName  
        let todoTasks = lines contents  
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
        putStr $ unlines numberedTasks  




getFileContent :: String -> IO Str.ByteString
getFileContent fileName = Str.readFile "in.txt"
 

main = view ["in.txt"]


-- main = do
--     putStrLn "Hello, choose the action: "
--     putStrLn "1) print file content"
--     putStrLn "2) append existing file"
--     putStrLn "3) delete a certain line"
--     putStrLn "4) copy file with filter"
--     putStrLn "Input the number of action: "
--     action <- getChar
--     if isDigit action then putStrLn "success" else putStrLn "fail"
