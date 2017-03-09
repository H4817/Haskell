

--replacePunctuationMarks :: IO ()


copyFile :: String -> String -> IO ()
copyFile f1 f2 = 
     putStrLn "Input symbol for replace punctuation marks: "
     >> getChar
     >>= \name -> putChar (name)


copyFileAndReplacePunctuationMarks :: String -> String -> IO ()
copyFileAndReplacePunctuationMarks f1 f2 =
     putStrLn "Input symbol for replace punctuation marks: "
     >> getChar
     >>= \name -> putChar (name)

main = copyFileAndReplacePunctuationMarks "f1" "f2"
