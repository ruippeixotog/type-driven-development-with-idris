module Chapter5_3

import Data.Vect

-- exercise 1
readToBlank : IO (List String)
readToBlank = do
  line <- getLine
  if line == ""
    then pure []
    else do
      lines <- readToBlank
      pure (line :: lines)

-- exercise 2
readAndSave : IO ()
readAndSave = do
  xs <- readToBlank
  filename <- getLine
  Right () <- writeFile filename (unlines xs)
    | Left err => printLn err
  pure ()

-- exercise 3
readVectFileHandle : (file : File) -> IO (Maybe (n ** Vect n String))
readVectFileHandle file = do
  False <- fEOF file
    | True => pure (Just (_ ** []))
  Right line <- fGetLine file
    | Left err => pure Nothing
  Just (_ ** lines) <- readVectFileHandle file
    | Nothing => pure Nothing
  pure (Just (_ ** (line :: lines)))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- openFile filename Read
    | Left err => pure (_ ** [])
  Just vecPair <- readVectFileHandle file
    | Nothing => do pure (_ ** [])
  closeFile file
  pure vecPair
