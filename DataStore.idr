module DataStore

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> 
              (items : Vect size String) ->
              DataStore
              
size : DataStore -> Nat              
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
           where
            addToData : Vect old String -> Vect (S old) String
            addToData [] = [newItem]
            addToData (x :: xs) = x :: addToData xs
            
data Command = Add String
             | Get Integer
             | Quit
   
total                     
parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing 
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

total
parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)
        
total
getEntry : (pos : Integer) -> (store : DataStore) -> (String, DataStore)
getEntry pos store = let storeItems = items store 
                     in case integerToFin pos (size store) of
                              Nothing => ("Index out of range.\n", store)
                              (Just id) => (index id storeItems ++ "\n", store)

total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command.\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => Just (getEntry pos store)
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
