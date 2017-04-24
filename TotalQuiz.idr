module TotalQuiz

import Data.Primitives.Views
import System

%default total

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO
     Stop : InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do action cont) = do x <- action
                                      run fuel (cont x)
run (More fuel) Stop = pure ()
run Dry _ = putStrLn "Out of fuel."

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
               
quiz : Stream Int -> Int -> InfIO
quiz randoms score = do 
     let (x :: y :: xs) = randoms
     putStr ((show x) ++ " * " ++ (show y) ++ " = ")
     answer <- getLine
     if all isDigit (unpack answer)
     then let answerNum = the Int (cast answer)
          in  if x * y == answerNum
                 then do putStrLn ("Correct! Score: " ++ show (score + 1))
                         quiz xs (score + 1)
                 else do putStrLn ("Wrong... the answer was " ++ show (x * y))
                         quiz xs score
     else case answer of
          "quit" => Stop
          _ => do putStrLn "Invalid input." 
                  quiz (x :: y :: xs) score

partial
main : IO ()
main = do seed <- time
          run forever (quiz (randoms (fromInteger seed)) 0)
