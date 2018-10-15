printLonger : IO ()
printLonger = do
    putStr "Enter first string: "
    first <- getLine
    putStr "Enter second string: "
    second <- getLine
    let firstLen = length first
    let secondLen = length second
    let longer = max firstLen secondLen
    putStrLn (show longer)

printLonger' : IO ()
printLonger' =
    putStr "Enter first string: " >>= \_ =>
    getLine >>= \first =>
    putStr "Enter second string: " >>= \_ =>
    getLine >>= \second =>
    let firstLen = length first
        secondLen = length second
        longer = max firstLen secondLen in
    putStrLn (show longer)
