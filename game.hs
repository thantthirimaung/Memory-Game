import System.Random
import Data.List (nub, sort)
import Control.Monad
import Data.IORef  -- Import if using IORef for global state
import System.IO

type Cell = (Char, Bool)  -- (Symbol, IsRevealed)
type Board = [[Cell]]

main :: IO ()
main = do
  putStrLn "Memory Game"
  putStrLn "1. Start New Game"
  putStrLn "2. Scores"
  putStrLn "3. Guide"
  putStrLn "4. Quit"
  putStr "Please pick your choice: "
  option <- getLine
  case option of
    "1" -> startGame
    "2" -> displayScores  
    "3" -> putStrLn "Memory Game Guide:\n\n\
                   \Welcome to our Memory Game. Here is the guide for you.\n\
                   \\n\
                   \* To start the new game : Choose Option 1\n\
                   \* To know how to see your scores : Choose Option 2\n\
                   \* To see Guide for the game (this session) : Choose Option 3\n\
                   \* To Quit from the game : Choose Option 4\n\
                   \* When starting the game, choose \"height\", \"width\", \"K\" and \"T\".\n\
                   \* Enter your move by entering the location of \"row\" then the location of \"column\".\n\
                   \* If you want to quit, enter 'q' or 'Q' to end the game.\n"
    "4" -> putStrLn "Exiting game. Goodbye!"
    _   -> putStrLn "Invalid option. Please choose again." >> main

startGame :: IO ()
startGame = do
  putStrLn "Enter height of the board: "
  height <- read <$> getLine
  when (height > 20) $ do
    putStrLn "Height is too large, please enter value of less than or equal 20."
    startGame

  putStrLn "Enter width of the board: "
  width <- read <$> getLine
  when (width > 20) $ do
    putStrLn "Width is too large, please enter value of less than or equal 20."
    startGame

  putStrLn "Enter K (number of symbol pairs): "
  k <- read <$> getLine
  when (k > 36) $ do
    putStrLn "Too many K values. K should be less than or equal 36."
  when (k * 2 > height * width) $ do
    putStrLn "Too many symbols for the board size. Please choose a smaller k."
    startGame


  putStrLn "Enter T (time delay in seconds): "
  t <- read <$> getLine
  let symbols = take k ['A'..'Z']  -- Get k unique symbols
  let pairs = symbols ++ symbols  -- Duplicate each symbol to create pairs
  shuffledPairs <- shuffle pairs
  let board = initializeBoard height width shuffledPairs
  playGame board t
  


initializeBoard :: Int -> Int -> [Char] -> Board
initializeBoard height width symbols =
  let flattenedBoard = take (height * width) symbols
      initialBoard = map (\s -> (s, False)) flattenedBoard
  in myChunksOf width initialBoard

myChunksOf :: Int -> [a] -> [[a]]
myChunksOf _ [] = []
myChunksOf n xs = take n xs : myChunksOf n (drop n xs)

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (x:xs) i
  | i == 0 = Just x
  | otherwise = atMay xs (i - 1)

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn $ "    " ++ concatMap (\i -> " " ++ show i ++ "  ") [0..width-1]
  putStrLn $ "   +" ++ replicate (width * 4 + 1) '-'
  mapM_ (\(i, row) -> putStrLn $ rowString i ++ "|" ++ concatMap cellString row) $ zip [0..] board
  putStrLn $ "   +" ++ replicate (width * 4 + 1) '-'
  where
    width = length (head board)
    height = length board
    rowString i = " " ++ show i ++ " |"
    cellString (s, r) = if r then " " ++ [s] ++ " |" else " * |"

--printBoard :: Board -> IO ()
--printBoard = mapM_ (putStrLn . map (\(s, r) -> if r then s else '*'))

revealCell :: Board -> Int -> Int -> Board
revealCell board row col = updateCell board row col (\(s, _) -> (s, True))

hideCell :: Board -> Int -> Int -> Board
hideCell board row col = updateCell board row col (\(s, _) -> (s, False))

updateCell :: Board -> Int -> Int -> (Cell -> Cell) -> Board
updateCell board row col f =
  let (before, row':after) = splitAt row board
      (beforeCol, cell:afterCol) = splitAt col row'
      updatedRow = beforeCol ++ [f cell] ++ afterCol
  in before ++ [updatedRow] ++ after

getCell :: Board -> Int -> Int -> Maybe Cell
getCell board row col = do
  row' <- board `atMay` row
  row' `atMay` col

shuffle :: [a] -> IO [a]
shuffle xs = do
  gen <- newStdGen
  return $ fst $ shuffle' gen xs

shuffle' :: StdGen -> [a] -> ([a], StdGen)
shuffle' gen [] = ([], gen)
shuffle' gen lst =
  let (index, newGen) = randomR (0, length lst - 1) gen
      (rest, selected:rest') = splitAt index lst
      (shuffled, finalGen) = shuffle' newGen (rest ++ rest')
  in (selected:shuffled, finalGen)

delaySeconds :: Int -> IO ()
delaySeconds n = replicateM_ (n * 10^6) (return ())

playGame :: Board -> Int -> IO ()
playGame board delay = playGame' board 0 delay 0  -- Start with 0 moves and 0 score

playGame' :: Board -> Int -> Int -> Int -> IO ()
playGame' board moves delay score =
  if all (\(_, revealed) -> revealed) (concat board)  -- Check if all cells are revealed
    then do
      putStrLn $ "Congratulations! You won in " ++ show moves ++ " moves with a total score of " ++ show score ++ "."
      saveScore moves score  -- Save the score to the file
      main  -- Return to the main menu
    else do
      printBoard board
      -- Get first move
      putStrLn "Enter first move - row index ('Q' or 'q' to quit): "
      input1 <- getLine
      case input1 of
        "q" -> putStrLn "Quitting game. Goodbye!"  -- Quit the game
        "Q" -> putStrLn "Quitting game. Goodbye!"  -- Quit the game (case-insensitive)
        _   -> do
          let row1 = if input1 `elem` ["q", "Q"] then -1 else read input1  -- Set to an invalid value if quitting
          if row1 == -1
            then putStrLn "Quitting game. Goodbye!"  -- Quit the game
            else do
              putStrLn "Enter first move - column index: "
              col1 <- read <$> getLine
              let selectedCell1 = getCell board row1 col1

              case selectedCell1 of
                Nothing -> putStrLn "Invalid cell. Try again." >> playGame' board moves delay score
                Just (symbol1, False) -> do
                  let updatedBoard1 = revealCell board row1 col1
                  printBoard updatedBoard1
                  -- Get second move
                  putStrLn "Enter second move - row index ('Q' or 'q' to quit): "
                  input2 <- getLine
                  case input2 of
                    "q" -> putStrLn "Quitting game. Goodbye!"  -- Quit the game
                    "Q" -> putStrLn "Quitting game. Goodbye!"  -- Quit the game (case-insensitive)
                    _   -> do
                      let row2 = if input2 `elem` ["q", "Q"] then -1 else read input2  -- Set to an invalid value if quitting
                      if row2 == -1
                        then putStrLn "Quitting game. Goodbye!"  -- Quit the game
                        else do
                          putStrLn "Enter second move - column index: "
                          col2 <- read <$> getLine
                          let selectedCell2 = getCell updatedBoard1 row2 col2
                          let updatedBoard2 = revealCell updatedBoard1 row2 col2
                          printBoard updatedBoard2
                          case selectedCell2 of
                            Nothing -> putStrLn "Invalid cell. Try again." >> playGame' (hideCell updatedBoard1 row1 col1) (moves + 1) delay score
                            Just (symbol2, False) ->
                              if symbol1 == symbol2
                                then do
                                  putStrLn "Match found! You scored 2 points."
                                  let updatedBoard3 = revealCell updatedBoard1 row2 col2
                                  playGame' updatedBoard3 (moves + 1) delay (score + 2)  -- Increase score by 2
                                else do
                                  putStrLn "No match. Try again."
                                  _ <- delaySeconds delay
                                  playGame' (hideCell updatedBoard1 row1 col1) (moves + 1) delay score
                            Just (_, True) -> putStrLn "Cell already revealed. Try again." >> playGame' updatedBoard1 (moves + 1) delay score
                        --Just (_, True) -> putStrLn "Cell already revealed. Try again." >> playGame' board (moves + 1) delay score
                Just (_, True) -> putStrLn "Cell already revealed. Try again." >> playGame' board moves delay score

saveScore :: Int -> Int -> IO ()
saveScore moves score = do
  putStrLn "Saving your score..."
  withFile "scores.txt" AppendMode $ \file -> do
    hPutStrLn file $ "Your moves: " ++ show moves ++ ", Your score: " ++ show score

displayScores :: IO ()
displayScores = do
  putStrLn "Displaying Previous Scores..."
  scores <- readScoresFromFile
  mapM_ putStrLn scores
  main


readScoresFromFile :: IO [String]
readScoresFromFile = do
  contents <- readFile "scores.txt"
  return $ lines contents

