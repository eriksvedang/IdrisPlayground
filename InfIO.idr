module Main -- InfiniteIO

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

total
loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg) 
                   (\_ => loopPrint msg)

total -- Should not be total according to the book?! (p. 307)
run : InfIO -> IO ()
run (Do action cont) = do x <- action
                          run (cont x)

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

total
runFueled : Fuel -> InfIO -> IO ()
runFueled (More fuel) (Do action cont) = do x <- action
                                            runFueled fuel (cont x)
runFueled Dry _ = putStrLn "Out of fuel."

partial
forever : Fuel
forever = More forever

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

prog : InfIO
prog = do s <- getLine
          putStrLn s
          prog

main : IO ()
main = runFueled forever (loopPrint "yeah")
