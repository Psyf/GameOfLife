import Data.Array

main = do

    -- utility functions
    let num_cols mtx = snd $ snd $ bounds mtx
    let num_rows mtx = fst $ snd $ bounds mtx

    let rowAsStr mtx x = [mtx ! (x, y) | y <- [0..(num_cols mtx)]]
    let mtxAsStr mtx = unlines [rowAsStr mtx x | x <- [0..(num_rows mtx)]]
    let printMtx mtx = putStr (mtxAsStr mtx)

    let activateCells xs mtx = mtx // [(x, '+') | x <- xs]
    let deactivateCells xs mtx = mtx // [(x, ' ') | x <- xs]
    let countNeighbours (x, y) mtx = foldr (\(i, j) acc -> if (mtx!(i, j))=='+' then acc+1 else acc) 0 [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]

    -- reproduction
    let toBeBorn mtx = filter (\(x, y) -> if (countNeighbours (x, y) mtx)==3 then True else False) [(i, j) | i <- [1..((num_rows mtx)-1)], j <- [1..((num_cols mtx)-1)]]
    -- over/under -population
    let toDie mtx = filter (\(x, y) -> let count = countNeighbours (x, y) mtx in if mtx!(x,y)=='+' then (if count<2 then True else if count > 3 then True else False) else False) [(i, j) | i <- [1..((num_rows mtx)-1)], j <- [1..((num_cols mtx)-1)]]
    -- going from t to t+1
    let takeStep state = activateCells (toBeBorn state) $ deactivateCells (toDie state) state

    -- simulate and print each step from t to t+n
    let simulate n curState = do
        if n <= 0 then return curState
        else
            let nextState = takeStep curState
            in do
                printMtx nextState
                simulate (n-1) nextState

    -- intialize blank state. Padding of length 1 on all sides
    -- TODO let user choose size of matrix
    let emptyMtx = (array ((0, 0), (21, 41)) [((i, j), ' ') | i <- [0..21], j <- [0..41]])
    let createBorders mtx =  mtx // ([((0, j), '-') | j <- [0..(num_cols mtx)]] ++ [((num_rows mtx, j), '-') | j <- [0..(num_cols mtx)]] ++ [((i, 0), '|') | i <- [0..(num_rows mtx)]] ++ [((i, (num_cols mtx)), '|') | i <- [0..(num_rows mtx)]])
    let blankState = createBorders emptyMtx

    -- initializing with certain patterns
    -- 1. TODO add more patterns 
    -- 2. TODO let user choose pattern using input 
    -- 3. TODO allow custom inputs
    let initState = activateCells [(10, 19), (10, 20), (10, 21)] blankState -- blinker pattern

    putStrLn "Initial State: "
    printMtx initState

    putStrLn "How many steps do you want to simulate this for?"
    num_steps <- getLine
    let n = (read num_steps :: Int)

    simulate n initState
