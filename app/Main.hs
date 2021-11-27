module Main where
    import ArithmeticParser (parseExpr, ParseError)
    import Basic (Except (..), Annotated (..))

    main :: IO ()
    main = do
        str <- getLine
        case parseExpr str of
            (Error e) -> print e 
            (Success tree) -> print tree
