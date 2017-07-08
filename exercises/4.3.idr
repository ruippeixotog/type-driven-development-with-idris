module Chapter4_3

import Data.Vect

-- store --

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

indexedItems : (store : DataStore) -> Vect (size store) (Fin (size store), String)
indexedItems (MkData size' items') = zip range items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size store) newitem = MkData _ (addToData store)
  where
    addToData : Vect oldsize String -> Vect (S oldsize) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (index id (items store) ++ "\n", store)

searchEntry : String -> DataStore -> List String
searchEntry str store =
  let indexed_items = toList (indexedItems store) in
    map mkEntry (filter hasSubstr indexed_items)
  where
    hasSubstr : (Fin k, String) -> Bool
    hasSubstr (_, item) = isInfixOf str item

    mkEntry : (Fin k, String) -> String
    mkEntry (k0, str) = (show (finToInteger k0)) ++ ". " ++ str

-- commands --

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val =
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand cmd (ltrim args)

-- repl --

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (Get pos) => getEntry pos store
    Just (Search str) => Just (unlines (searchEntry str store), store)
    Just Size => Just (show (size store), store)
    Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
