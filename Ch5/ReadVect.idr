import Data.Vect

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

readVect : IO (VectUnknown String)
readVect = do x <- getLine
              if (x == "")
                 then pure (MkVect _ [])
                 else do MkVect _ xs <- readVect
                         pure (MkVect _ (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs)
      = putStrLn (show xs ++ " (length " ++ show len ++ ")")

anyVect : (n ** Vect n String)
anyVect = (2 ** ["hello", "world"])

-- I wanted to see whether we really needed VectUnknown. The type error from the
-- below isn't very clear, but there is indeed an error to do with vector length.
-- myNaiveReadVect : IO (Vect len String)
-- myNaiveReadVect = do x <- getLine
--                      if (x == "")
--                         then pure []
--                         else do xs <- myNaiveReadVect
--                                 pure (x :: xs)
