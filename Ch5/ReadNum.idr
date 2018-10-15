total readNumber : IO (Maybe Nat)
readNumber = do
    str <- getLine
    if all isDigit (unpack str)
       then pure (Just (cast str))
       else pure Nothing

total readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do num1 <- readNumber
     case num1 of
          Nothing => pure Nothing
          Just num1_ok =>
               do num2 <- readNumber
                  case num2 of
                       Nothing => pure Nothing
                       Just num2_ok => pure (Just (num1_ok, num2_ok))

total readNumbers' : IO (Maybe (Nat, Nat))
readNumbers' =
  do Just num1 <- readNumber | Nothing => pure Nothing
     Just num2 <- readNumber | Nothing => pure Nothing
     pure (Just (num1, num2))
