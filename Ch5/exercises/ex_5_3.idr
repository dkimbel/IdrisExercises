import Data.Vect

readToBlank : IO (List String)
readToBlank = do s <- getLine
                 if s == ""
                    then pure []
                    else do ss <- readToBlank
                            pure (s :: ss)

readAndSave : IO ()
readAndSave = do putStrLn "Enter values (blank to end):"
                 ss <- readToBlank
                 putStrLn "Enter path of file to save to:"
                 fPath <- getLine
                 let fContents = concat (intersperse "\n" ss)
                 Right _ <- writeFile fPath fContents
                   | Left err => do putStrLn (show err)
                                    pure ()
                 putStrLn ("Saved contents to " ++ fPath)

getFileContents: (file : File) -> IO (List String)
getFileContents file = do False <- fEOF file
                            | True => pure []
                          Right s <- fGetLine file
                            | Left err => pure []
                          ss <- getFileContents file
                          pure (s :: ss)

listToVecPair : List a -> (n ** Vect n a)
listToVecPair [] = (_ ** [])
listToVecPair (x :: xs) = (_ ** (x :: snd (listToVecPair xs)))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right file <- openFile filename Read
                             | Left err => do putStrLn (show err)
                                              pure (_ ** [])
                           ss <- getFileContents file
                           closeFile file
                           pure (listToVecPair ss)
