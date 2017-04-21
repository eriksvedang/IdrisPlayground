module ChangingType

-- My own version of the data store with changing schema,
-- just to understand it a bit better.

data KindOfThing = AnInt
                 | AString

ThingType : KindOfThing -> Type
ThingType AnInt = Int
ThingType AString = String

data Thing : Type where
     MkThing : (t : KindOfThing) -> (a : (ThingType t)) -> Thing
     
showTing : Thing -> String
showTing (MkThing AnInt a) = show a ++ " (Int)"
showTing (MkThing AString a) = a ++ " (String)"
     
Show Thing where
     show thing = "Thing" ++ (showTing thing)
     
loop : Thing -> IO ()
loop thing = 
     do putStrLn (show thing)
        s <- getLine
        if all isDigit (unpack s)
        then loop (MkThing AnInt (cast s)) 
        else loop (MkThing AString s)           

main : IO ()
main = loop (MkThing AnInt 100)
