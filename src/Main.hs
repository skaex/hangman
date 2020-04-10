module Main (Puzzle(..), fillInCharacter, handleGuess, main) where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle =
  Puzzle String [Maybe Char] String Int deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed aleft) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++
    " Guessed so far: " ++ guessed ++ " Guesses left: " ++ show aleft

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxGuesses :: Int
maxGuesses = 7

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) [] maxGuesses

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) = (`elem` s)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gs _) = (`elem` gs)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

deductGuessCount :: Puzzle -> Puzzle
deductGuessCount (Puzzle word discovered guessed guessLeft) = 
  Puzzle word discovered guessed (guessLeft - 1)

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s a) c =
  Puzzle word newFilledInSoFar (c : s) a
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
                 \ character, pick something else! Penalizing you!"
      return (deductGuessCount puzzle)
    (True, _) -> do
      putStrLn
        "This character was in the word,\
                 \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\ \ the word, try again. Penalizing you!"
      return $ deductGuessCount (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ guessesLeft) =
  when (guessesLeft <= 0) $
    do  putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  when (all isJust filledInSoFar) $
    do
      putStrLn "You win!"
      exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must\
                    \ be a single character"

gameWords :: IO WordList
gameWords = do 
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
