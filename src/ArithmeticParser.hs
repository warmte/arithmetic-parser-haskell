{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ArithmeticParser where
    import Basic (Except (..), Annotated (..))
    import SimpleEvaluator (Expr (..), Prim (..))
    import ExceptEvaluator (ExceptState (..), runES)
    import GHC.Natural ( Natural )
    import Control.Applicative (Alternative (..), optional)
    import Control.Monad (MonadPlus, mfilter, msum, when, unless, void)
    import Data.Char (isDigit, ord, isSpace)
    import qualified Data.Scientific as Scientific
    import Data.Scientific (Scientific, toRealFloat, scientific)
    import Data.Maybe (fromMaybe)
    
    data ParseError = ErrorAtPos Natural deriving Show -- parsing error with the position where it apeared.

    -- String parser which allows to throw a ParseError.
    newtype Parser a = P (ExceptState ParseError (Natural, String) a)
        deriving newtype (Functor, Applicative, Monad)

    -- Run given parser on a given string.
    runP :: Parser a -> String -> Except ParseError a
    runP (P eState) input = case runES eState (0, input) of
        (Error x)          -> Error x
        (Success (x :# _)) -> Success x

    -- Parse one character from input string.
    pChar :: Parser Char
    pChar = P $ ES \(pos, s) ->
        case s of
            []     -> Error (ErrorAtPos pos)
            (c:cs) -> Success (c :# (pos + 1, cs))

    -- Parse end of input string.
    pEof :: Parser ()
    pEof = P $ ES \(pos, s) ->
                if null s
                    then Success (() :# (pos, s))
                    else Error (ErrorAtPos pos)

    -- What happens when the string is empty?
    -- When the string is empty there'll be returned an Error constructed
    -- by ErrorAtPos and lenght of the string.

    -- How does the parser state change when a character is consumed?
    -- It annotates the character with the tuple of position and the rest part of the string.

    -- Throw ParseError.
    parseError :: Parser a
    parseError = P $ ES \(pos, s) -> Error (ErrorAtPos pos)

    -- | Alternative instance of Parser. Tries to run the left parser and then right parser
    -- if left one has failed.
    instance Alternative Parser where
        empty = parseError
        (<|>) (P p) (P q) = P $ ES \(pos, s) ->
            case runES p (pos, s) of
                (Success f) -> Success f
                (Error _)   -> runES q (pos, s)

    -- MonadPlus instance of Parser.
    instance MonadPlus Parser   -- No methods.

    -- Parser which reads spaces and skips them.
    skipSpaces :: Parser ()
    skipSpaces = do
        many (mfilter isSpace pChar)
        pure ()

    -- Parser which reads next non-space character.
    pSym :: Char -> Parser Char
    pSym c = do
        skipSpaces
        mfilter (== c) pChar

    -- Parser which reads a sequence of symbols satisfying given predicate.
    pStr :: (Char -> Bool) -> Parser String
    pStr f = some (mfilter f pChar)

    -- Transform given string to double.
    stringToDigit :: String -> Int -> Double
    stringToDigit x len   | len == 0 = fromInteger (fst (stringToInt x))
                          | otherwise = toRealFloat (scientific (fst (stringToInt x)) (-len))
        where 
            stringToInt :: String -> (Integer, Int)
            stringToInt "" = (0, 1)
            stringToInt (x:xs) = (res + toInteger ((ord x - ord '0') * base), base * 10)
                where (res, base) = stringToInt xs

    -- Take operation symbol and return the function which construct matching Expr.
    matchOp :: Char -> Expr -> Expr -> Expr
    matchOp '+' = \x y -> Op (Add x y)
    matchOp '-' = \x y -> Op (Sub x y)
    matchOp '*' = \x y -> Op (Mul x y)
    matchOp '/' = \x y -> Op (Div x y)
    matchOp c   = undefined

    -- Parser which parses two operations with the same priority: + and -, or * and /.
    parser' :: (Char, Char) -> Parser Expr -> Parser (Expr, Expr -> Expr -> Expr)
    parser' (c1, c2) parser = do
        op <- pSym c1 <|> pSym c2
        unless (op `elem` "+-*/") parseError
        x <- parser
        pure (x, matchOp op)

    -- Take sequence of equal priority operation and return the result Expression.
    compose :: Expr -> [(Expr, Expr -> Expr -> Expr)] -> Expr
    compose x []            = x
    compose x ((ex, cr):ys) = compose (cr x ex) ys

    -- Creates parser with given equal priority operations and a matching parser for them.
    createParser :: (Char, Char) -> Parser Expr -> Parser Expr
    createParser (c1, c2) parser = do
        x <- parser
        y <- many (parser' (c1, c2) parser)
        pure (compose x y)

    -- Parser to parse an expression overall.
    exprParser :: Parser Expr
    exprParser = do
        x <- addSubParser
        skipSpaces
        pEof
        pure x

    -- Parser to parse addition and subtruction.
    addSubParser :: Parser Expr
    addSubParser = createParser ('+', '-') mulDivParser

    -- Parser to parse multiplication and division.
    mulDivParser :: Parser Expr
    mulDivParser = createParser ('*', '/') valParser

    -- Parser to parse unite value like numeric one or expression surrounded by brackets.
    valParser :: Parser Expr
    valParser = parseNum <|> bracketParser

    -- Parser to parse double values.
    parseNum :: Parser Expr
    parseNum = do
        skipSpaces
        start <- pStr isDigit
        optional (mfilter (== '.') pChar)
        end <- optional (pStr isDigit)
        pure (Val (stringToDigit (start ++ fromMaybe ""  end) (length (fromMaybe "" end))))

    -- Parser to parse an expression surrounded by brackets.
    bracketParser :: Parser Expr
    bracketParser = do
        void (pSym '(')
        x <- addSubParser
        void (pSym ')')
        pure x

    -- Parse given string and return Expr.
    parseExpr :: String -> Except ParseError Expr
    parseExpr = runP exprParser

