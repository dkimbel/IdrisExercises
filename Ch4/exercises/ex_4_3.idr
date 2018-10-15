module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

total size : DataStore -> Nat
size (MkData n _) = n

total items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

total addToStore : DataStore -> String -> DataStore
addToStore (MkData _ items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item' :: items') = item' :: addToData items'

total parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = case (str == "") of
                                 False => Just (Search str)
                                 True => Nothing
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

total parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

total getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
                          Nothing => Just ("Out of range\n", store)
                          Just n => let item = index n (items store) in
                                        Just (item ++ "\n", store)

-- it would have been slick to have returned a tuple containing a Nat instead of an Int, but
-- I just don't know enough Idris yet to make the types fit together
total getSearchResults : (store : DataStore) -> (query : String) -> (p : Nat ** Vect p (Int, String))
getSearchResults (MkData size items) query =
    Vect.filter (\(n, item) => isInfixOf query item) (toNumberedItems items)
      where
        toNumberedItems : Vect n String -> Vect n (Int, String)
        toNumberedItems [] = []
        toNumberedItems {n = S k} (item' :: items') = (cast size - cast (S k), item') :: toNumberedItems items'

total formatSearchResults : (p : Nat ** Vect p (Int, String)) -> String
formatSearchResults (_ ** pairs) = concat (intersperse "\n" (showPairs pairs)) ++ "\n"
  where
    showPairs : Vect p (Int, String) -> Vect p String
    showPairs inp = map (\(n, str) => show n ++ ": " ++ str) inp

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store str = case (parse str) of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos)) => getEntry pos store
                              (Just (Search query)) => Just (formatSearchResults (getSearchResults store query), store)
                              (Just Size) => Just (show (size store) ++ " entries stored\n", store)
                              (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
