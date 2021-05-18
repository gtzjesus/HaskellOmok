module Main where

    import System.IO
    import Board

    -- Creating the 15x15 board & player 1 as initial player for game
    main = do
        putStrLn (boardToStr playerToChar (mkBoard 15) 1)
        readXY (mkBoard 15) mkPlayer

    -- readXY function; User inputs appropriate values for X and Y respectively.
    -- If input = -1, the user exits the game.
    -- If input is out of the board boundaries, the user is requested to input again.
    readXY:: [[Int]] -> Int -> IO ()
    readXY bd p = do
        putStrLn ((playerToChar p) ++ "'s turn: Enter X (1-15) - or Enter -1 to end game")
        line <- getLine
        let x = (read line :: Int) in
            if x > 0 && x <= 15 then do
                putStrLn ((playerToChar p) ++ "'s turn: Enter Y (1-15) - or Enter -1 to end game")
                line <- getLine
                let y = (read line :: Int) in
                    if y > 0 && y <= 15 then
                        if isMarked x y bd then do
                            putStrLn "This place is occupied."
                            readXY bd p
                        else do
                            let gameBoard = mark x y bd p
                            putStrLn (boardToStr playerToChar gameBoard 1)
                            if isWonBy gameBoard p then
                                putStrLn ((playerToChar p) ++ " is the WINNER.")
                            else if isDraw gameBoard then
                                putStrLn "The game has come to a draw"
                            else
                                if p == 1 then
                                    readXY gameBoard mkOpponent
                                else
                                    readXY gameBoard mkPlayer
                    else if y == -1 then do
                        putStrLn "Come back again soon."
                        return ()
                    else
                        readXY'
            else if x == -1 then do
                putStrLn "Come back again soon."
                return ()
            else 
                readXY'                                   
        where
            readXY' = do
                putStrLn "Invalid input."
                readXY bd p