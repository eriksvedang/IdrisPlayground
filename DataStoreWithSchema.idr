module DataStoreWithSchema

import Data.Vect

%default total

infixr 5 .+.

data Schema = SString 
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
     constructor MkData 
     schema : Schema
     size : Nat
     items : Vect size (SchemaType schema)
    
teststore : DataStore
teststore = MkData (SString .+. SInt) 0 []

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
           where
            addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
            addToData [] = [newItem]
            addToData (x :: xs) = x :: addToData xs
            
data Command : Schema -> Type where
     SetSchema : (newSchema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Quit : Command schema
   
parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
            where
                getQuoted : List Char -> Maybe (String, String)
                getQuoted ('"' :: xs) = case span (/= '"') xs of
                                             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                             _ => Nothing
                getQuoted _ = Nothing
                
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemaL .+. schemaR) input = do (l_val, input') <- parsePrefix schemaL input
                                             (r_val, input'') <- parsePrefix schemaR input'
                                             pure ((l_val, r_val), input'')
   
parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _ => case parseSchema xs of
                                              Nothing => Nothing
                                              Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _ => case parseSchema xs of
                                           Nothing => Nothing
                                           Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      (Just restok) => Just (Add restok)
parseCommand schema "get" val = case all isDigit (unpack val) of
                              False => Nothing 
                              True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = case parseSchema (words rest) of
                                         Nothing => Nothing
                                         Just schemaOK => Just (SetSchema schemaOK)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)
        
display : SchemaType schema -> String        
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (item1, item2) = display item1 ++ ", " ++ display item2
        
getEntry : (pos : Integer) -> (store : DataStore) -> (String, DataStore)
getEntry pos store = let storeItems = items store 
                     in case integerToFin pos (size store) of
                              Nothing => ("Index out of range.\n", store)
                              (Just id) => (display (index id storeItems) ++ "\n", store)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command.\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => Just (getEntry pos store)
                                Just Quit => Nothing
                                Just (SetSchema schema') => case setSchema store schema' of
                                                                 Just store' => Just ("OK\n", store')
                                                                 Nothing => Just ("Can't update schema.\n", store)

partial
main : IO ()
main = replWith (MkData (SString .+. SInt) _ []) "Command: " processInput
