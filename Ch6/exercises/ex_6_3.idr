module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

data Command : Schema -> Type where
     SetSchema : (newschema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     GetAll : Command schema
     Quit : Command schema

-- why did we need to parameterize Command with Schema, instead of
-- just using something like the below?
-- data Command : Type where
--      Add : SchemaType schema -> Command
--      Get : Integer -> Command
--      Quit : Command

total parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) =
    case xs of
         [] => Just SString
         _ => do xs_sch <- parseSchema xs
                 pure (SString .+. xs_sch)
parseSchema ("Int" :: xs) =
    case xs of
         [] => Just SInt
         _ => do xs_sch <- parseSchema xs
                 pure (SInt .+. xs_sch)
parseSchema ("Char" :: xs) =
    case xs of
         [] => Just SChar
         _ => do xs_sch <- parseSchema xs
                 pure (SChar .+. xs_sch)
parseSchema _ = Nothing

total addToStore : (store: DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema _ items) newItem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newItem]
    addToData (item' :: items') = item' :: addToData items'

total parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) =
        case span (/= '"') xs of
             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
             _ => Nothing
    getQuoted _ = Nothing

parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)

parsePrefix SChar input = case span (/= ' ') (unpack input) of
                               (char :: [], rest) => Just (char, ltrim (pack rest))
                               _ => Nothing

parsePrefix (schemal .+. schemar) input =
    do (l_val, input')  <- parsePrefix schemal input
       (r_val, input'') <- parsePrefix schemar input'
       pure ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

total parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand _ "schema" rest = do newschema <- parseSchema (words rest)
                                  pure (SetSchema newschema)
parseCommand schema "add" rest = do schemaType <- parseBySchema schema rest
                                    pure (Add schemaType)
parseCommand _ "get" val = case all isDigit (unpack val) of
                                False => Nothing
                                True => case val of
                                             "" => Just GetAll
                                             _ => Just (Get (cast val))
parseCommand _ "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

total parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

total setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

total display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

total getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
                          Nothing => Just ("Out of range\n", store)
                          Just n => let item = index n (items store) in
                                        Just (display item ++ "\n", store)

total getAllEntries : (store : DataStore) -> Maybe (String, DataStore)
getAllEntries store =
    case size store of
         Z => Just ("Store is empty\n", store)
         _ => Just (contentsAsStr (items store), store)
    where
      -- can this be modified easily to include the row's id?
      contentsAsStr : (contents : Vect (size store) (SchemaType (schema store))) -> String
      contentsAsStr contents = foldr (++) "" stringsWithIds
        where
          strings : Vect (size store) String
          strings = map (\item => display item ++ "\n") contents
          idStrings : Vect (size store) String
          idStrings = map show (reverse (buildIdVect (size store)))
            where
              -- This is backwards, so I have to call `reverse` on it above.
              -- And indeed, overall, this is pretty inelegant. If I'd noticed the requirement to show
              -- an id beside each string from the outset, I would've done something like I did
              -- in my filtering method on a previous chapter's exercise (Ch4?), and used a dependent
              -- pair of a vector and its length to do my logic in one go, rather than mapping and folding.
              buildIdVect : (n : Nat) -> Vect n Nat
              buildIdVect Z = []
              buildIdVect (S k) = k :: buildIdVect k
          stringsWithIds : Vect (size store) String
          stringsWithIds = zipWith (\s1, s2 => s1 ++ ": " ++ s2) idStrings strings

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (SetSchema schema) =>
                                      case setSchema store schema of
                                           Nothing => Just ("Cannot update schema unless store is empty\n", store)
                                           Just newStore => Just ("OK\n", newStore)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => getEntry pos store
                                Just GetAll => getAllEntries store
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ [])
                "Command: " processInput
