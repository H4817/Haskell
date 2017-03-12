import System.IO
import qualified Data.ByteString as Str
import Data.Char
import System.Environment   
import System.Directory  
import Data.List  
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

-- dispatch :: [(String, [String] -> IO ())]  
-- dispatch =  [ ("add", add)  
--             , ("view", view)  
--             , ("remove", remove)  
--             ]  


add :: String -> String -> IO ()  
add fileName todoItem = appendFile fileName (todoItem ++ "\n")  


view :: String -> IO ()  
view fileName = do  
        contents <- readFile fileName  
        let todoTasks = lines contents  
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
        putStr $ unlines numberedTasks  


remove :: String -> Int -> IO ()  
remove fileName numberString = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! numberString) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  
 

main = do
    --remove "in.txt" 15
    view "in.txt"


-- main = do
--     putStrLn "Hello, choose the action: "
--     putStrLn "1) print file content"
--     putStrLn "2) append existing file"
--     putStrLn "3) delete a certain line"
--     putStrLn "4) copy file with filter"
--     putStrLn "Input the number of action: "
--     action <- getChar
--     if isDigit action then putStrLn "success" else putStrLn "fail"
