import Data.Array

main = do

    putStrLn "How many rows do you want?"
    num_rows <- getLine 
    let x = (read num_rows :: Int)

    putStrLn "How many columns do you want?"
    num_cols <- getLine 
    let y = (read num_cols :: Int)

    -- 1. TODO add more patterns
    -- 2. TODO let user choose pattern using input 
    -- 3. TODO allow custom inputs
    let blinker = [(10, 19), (10, 20), (10, 21)] -- blinker pattern

    let initState = initializeMtx x y blinker

    putStrLn "Initial State: "
    printMtx initState

    putStrLn "How many steps do you want to simulate this for?"
    num_steps <- getLine
    let n = (read num_steps :: Int)

    simulate n initState

-- simulate and print each step from t to t+n
simulate n curState = do
    let countNeighbours (x, y) mtx = foldr (\(i, j) acc -> if (mtx!(i, j))=='+' then acc+1 else acc) 0 [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]
    let toBeBorn mtx = filter (\(x, y) -> if (countNeighbours (x, y) mtx)==3 then True else False) [(i, j) | i <- [1..((num_rows mtx)-1)], j <- [1..((num_cols mtx)-1)]]
    let toDie mtx = filter (\(x, y) -> let count = countNeighbours (x, y) mtx in if mtx!(x,y)=='+' then (if count<2 then True else if count > 3 then True else False) else False) [(i, j) | i <- [1..((num_rows mtx)-1)], j <- [1..((num_cols mtx)-1)]]
    let takeStep state = activateCells (toBeBorn state) $ deactivateCells (toDie state) state

    if n <= 0 then return curState
    else
        let nextState = takeStep curState
        in do
            printMtx nextState
            simulate (n-1) nextState

-- utility functions
num_cols mtx = snd $ snd $ bounds mtx
num_rows mtx = fst $ snd $ bounds mtx

rowAsStr mtx x = [mtx ! (x, y) | y <- [0..(num_cols mtx)]]
mtxAsStr mtx = unlines [rowAsStr mtx x | x <- [0..(num_rows mtx)]]
printMtx mtx = putStr (mtxAsStr mtx)

activateCells xs mtx = mtx // [(x, '+') | x <- xs]
deactivateCells xs mtx = mtx // [(x, ' ') | x <- xs]


initializeMtx n m set_cells =
    -- intialize blank state. Padding of length 1 on all sides
    let emptyMtx = (array ((0, 0), (n+1, m+1)) [((i, j), ' ') | i <- [0..(n+1)], j <- [0..(m+1)]]) 
    in
      let createBorders mtx = mtx // ([((0, j), '-') | j <- [0..(num_cols mtx)]] ++ [((num_rows mtx, j), '-') | j <- [0..(num_cols mtx)]] ++ [((i, 0), '|') | i <- [0..(num_rows mtx)]] ++ [((i, (num_cols mtx)), '|') | i <- [0..(num_rows mtx)]])
      in activateCells set_cells (createBorders emptyMtx)
