import Intcode
import Control.Monad.Trans.State

main = do
    a <- readFile "11.txt"
    let b = execState (run $ read a) (Computer (Program []) 0 0 0)
    return b

run :: [Int] -> State Computer Code
run s = do
    setState s
    currstate <- get
    execCode >> execCode >> execCode >> execCode

setState :: [Int] -> State Computer Code
setState code = do
    let computer = Computer (Program code) 0 0 0
    put computer
    return (Program code) 

execCode :: State Computer Code
execCode = do
    currentState <- get
    let (code, newState) = runCode currentState
    put newState
    return code
