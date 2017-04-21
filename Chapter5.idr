module Chapter5

import Data.Vect

readVect : IO (n ** Vect n String)
readVect = do input <- getLine
              case input of
                   "" => pure (_ ** [])
                   x  => do (_ ** xs) <- readVect
                            pure (_ ** x :: xs)

readTwoVectsAndZipThem : IO ()
readTwoVectsAndZipThem = do putStrLn "First vector: "
                            (len1 ** vec1) <- readVect
                            putStrLn "Second vector: "
                            (len2 ** vec2) <- readVect
                            case exactLength len1 vec2 of
                                 Just vec2exact => putStrLn (show (zip vec1 vec2exact))
                                 Nothing => putStrLn "Can't zip them, different lengths..." 
