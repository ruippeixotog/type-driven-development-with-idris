module Chapter6_3

import Data.Vect

-- store --

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (sc1 .+. sc2) = (SchemaType sc1, SchemaType sc2)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)


addToStore : (store : DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData schema size items) newitem = MkData _ _ (items ++ [newitem])

display : SchemaType schema -> String
display {schema = SString} str = "\"" ++ str ++ "\""
display {schema = SInt} k = show k
display {schema = SChar} ch = show ch
display {schema = (sc1 .+. sc2)} (v1, v2) = (display v1) ++ ", " ++ (display v2)

getAllEntries : DataStore -> List String
getAllEntries store = toList (map mkEntry indexedItems)
  where
    indexedItems : Vect (size store) (Fin (size store), SchemaType (schema store))
    indexedItems = zip range (items store)

    mkEntry : (Fin k, SchemaType sc) -> String
    mkEntry (k0, v) = (show (finToInteger k0)) ++ ". " ++ (display v)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (display (index id (items store)) ++ "\n", store)

-- commands --

data Command : Schema -> Type where
  Add : SchemaType schema -> Command schema
  GetAll : Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)

parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) =
      case span (/= '"') xs of
        (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
    getQuoted _ = Nothing

parsePrefix SInt input = case span isDigit input of
                           ("", rest) => Nothing
                           (num, rest) => Just (cast num, ltrim rest)

parsePrefix SChar input = case unpack input of
                            '\'' :: ch :: '\'' :: rest => Just (ch, ltrim (pack rest))
                            _ => Nothing

parsePrefix (sc1 .+. sc2) input = do
  (v1, input') <- parsePrefix sc1 input
  (v2, input'') <- parsePrefix sc2 input'
  pure ((v1, v2), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                               Just (v, "") => Just v
                               _ => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" str = map (\a => Add a) (parseBySchema schema str)
parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" val =
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)

-- repl --

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just GetAll => Just (unlines $ getAllEntries store, store)
    Just (Get pos) => getEntry pos store
    Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
