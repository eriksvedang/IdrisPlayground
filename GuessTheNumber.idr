module GuessTheNumber

total
onInput : (state : Nat) -> (input : String) -> Maybe (Nat, String)
onInput state input = case compare (cast input) state of
                           LT => Just (state, "Too low")
                           EQ => Nothing
                           GT => Just (state, "Too high")
   
interactiveProgramLoop : (state : s) -> String -> (s -> String -> (Maybe (s, String))) -> IO ()
interactiveProgramLoop state prompt f = 
  do putStr prompt
     input <- getLine
     case f state input of
          Nothing => putStrLn "Correct!"
          Just (newState, output) =>
               do putStrLn output
                  interactiveProgramLoop newState prompt f

main : IO ()
main = interactiveProgramLoop 42 "Your guess: " onInput
