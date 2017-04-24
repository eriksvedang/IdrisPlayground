module Quiz -- p. 301

import Data.Primitives.Views

bound : Int -> Int
bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 
                   number = seed' `shiftR` 2
               in bound number :: randoms seed'
               
quiz : Stream Int -> Int -> IO ()
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
     else do putStrLn "Invalid input." 
             quiz (x :: y :: xs) score

main : IO ()
main = quiz (randoms 100) 0
