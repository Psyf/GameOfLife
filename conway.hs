import Data.Array
import Control.Concurrent

main = do

    -- TODO guard against malicious input sequences

    putStrLn "How many rows do you want?"
    num_rows <- getLine 
    let x = (read num_rows :: Int)

    putStrLn "How many columns do you want?"
    num_cols <- getLine 
    let y = (read num_cols :: Int)

    putStrLn "Type a pattern name from (blinker, toad, beacon, pulsar, penta-decathlon, glider, lightweight-spaceship, middleweight-spaceship, heavyweight-spaceship, random)"
    pattern_str <- getLine

    let initState = initializeMtx x y (generatePattern pattern_str (x `div` 2) (y `div` 2))

    putStrLn "Initial State: "
    printMtx initState

    putStrLn "How many steps do you want to simulate this for?"
    num_steps <- getLine
    let n = (read num_steps :: Int)

    putStrLn "How much delay do you want between each step (microseconds)?"
    raw_delay <- getLine
    let delay = (read raw_delay :: Int)

    simulate n initState delay

-- simulate and print each step from t to t+n
simulate n curState delay = do
    let countNeighbours (x, y) mtx = foldr (\(i, j) acc -> if (mtx!(i, j))==alive then acc+1 else acc) 0 [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]
    let toBeBorn mtx = filter (\(x, y) -> if (mtx!(x,y)==blank && (countNeighbours (x, y) mtx)==3) then True else False) [(i, j) | i <- [1..((num_rows mtx)-1)], j <- [1..((num_cols mtx)-1)]]
    let toDie mtx = filter (\(x, y) -> let count = countNeighbours (x, y) mtx in if (mtx!(x,y)==alive && count /= 2 && count /= 3) then True else False) [(i, j) | i <- [1..((num_rows mtx)-1)], j <- [1..((num_cols mtx)-1)]]
    let takeStep state = activateCells (toBeBorn state) $ deactivateCells (toDie state) state

    if n <= 0 then return curState
    else
        let nextState = takeStep curState
        in do
            printMtx nextState
            threadDelay delay
            simulate (n-1) nextState delay

-- utility functions
num_cols mtx = snd $ snd $ bounds mtx
num_rows mtx = fst $ snd $ bounds mtx

blank = ' '
alive = '+'

rowAsStr mtx x = [mtx ! (x, y) | y <- [0..(num_cols mtx)]]
mtxAsStr mtx = unlines [rowAsStr mtx x | x <- [0..(num_rows mtx)]]
printMtx mtx = putStr (mtxAsStr mtx)

activateCells xs mtx = mtx // [(x, alive) | x <- xs]
deactivateCells xs mtx = mtx // [(x, blank) | x <- xs]

initializeMtx n m set_cells =
    -- intialize blank state. Padding of length 1 on all sides
    let emptyMtx = (array ((0, 0), (n+1, m+1)) [((i, j), blank) | i <- [0..(n+1)], j <- [0..(m+1)]]) 
    in
      let createBorders mtx = mtx // ([((0, j), '-') | j <- [0..(num_cols mtx)]] ++ [((num_rows mtx, j), '-') | j <- [0..(num_cols mtx)]] ++ [((i, 0), '|') | i <- [1..(num_rows mtx)-1]] ++ [((i, (num_cols mtx)), '|') | i <- [1..(num_rows mtx)-1]])
      in activateCells set_cells (createBorders emptyMtx)

-- 1. TODO add more patterns
-- 2. TODO allow custom inputs
generatePattern "blinker" x y =  [(x, y-1), (x, y), (x, y+1)]
generatePattern "toad" x y = [(x, y-1), (x, y), (x, y+1), (x+1, y), (x+1, y-1), (x+1, y-2)]
