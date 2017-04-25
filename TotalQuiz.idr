module TotalQuiz

import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
     Stop : a -> ConsoleIO a

data Fuel = Dry | More (Lazy Fuel)

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind f g) = do x <- runCommand f
                           runCommand (g x)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run (More fuel) (Do action cont) = do x <- runCommand action
                                      run fuel (cont x)
run (More fuel) (Stop val) = pure (Just val)
run Dry _ = pure Nothing

namespace CommandDo
          (>>=) : Command a -> (a -> Command b) -> Command b
          (>>=) = Bind
          
namespace ConsoleDo
          (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
          (>>=) = Do

partial
forever : Fuel
forever = More forever

bound : Int -> Int
bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 
                   number = seed' `shiftR` 2
               in bound number :: randoms seed'
               
readInput : String -> Command String
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      Pure answer
               
quiz : Stream Int -> Int -> ConsoleIO Int
quiz randoms score = do 
     let (x :: y :: xs) = randoms
     answer <- readInput ((show x) ++ " * " ++ (show y) ++ " = ")
     if all isDigit (unpack answer)
     then let answerNum = the Int (cast answer)
          in  if x * y == answerNum
                 then do PutStr ("Correct! Score: " ++ show (score + 1) ++ "\n")
                         quiz xs (score + 1)
                 else do PutStr ("Wrong... the answer was " ++ show (x * y) ++ "\n")
                         quiz xs score
     else case answer of
          "quit" => Stop score
          _ => do PutStr "Invalid input.\n" 
                  quiz (x :: y :: xs) score

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (randoms (fromInteger seed)) 0) | Nothing => putStrLn "Out of fuel."
          putStrLn ("Final score: " ++ show score)
