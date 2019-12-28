import Intcode

codeLoop :: Computer -> IO ()
codeLoop comp@(Computer code offset base input) = do
    let comp'@(Computer code' offset' base' _) = runCode comp
    case code' of Output c'  -> do 
                            putStrLn "Output Number:"
                            print code'
                            codeLoop (Computer code offset' base' input)
                  Pause c'   -> do
                            putStrLn "Input Number:"
                            i <- getLine
                            let comp' = runCode (Computer code offset base (read i))
                            codeLoop comp'          
                  Done       -> putStrLn "Done"
                  Program c' -> codeLoop comp'

main = do
    c <- readFile "9.txt"
    let code = read c :: [Int]
        computer = Computer (Program (code ++ repeat 0)) 0 0 0
    codeLoop computer