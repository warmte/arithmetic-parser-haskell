module SimpleEvaluator where
    import Basic (Annotated (..))
    import qualified Control.Monad

    -- State monad with annotation function.
    data State s a = S { runS :: s -> Annotated s a }

    -- Get annotated object and return it without note.
    unAnnotate :: Annotated s a -> a
    unAnnotate (x :# xs) = x

    -- Get annotated object and return its note.
    getAnnotation :: Annotated s a -> s
    getAnnotation (x :# xs) = xs

    -- Construct State with the annotation function which modifies the given object using the given function.
    mapState :: (a -> b) -> State s a -> State s b
    mapState f state = S {runS = \x -> f (unAnnotate (runS state x)) :# getAnnotation (runS state x)}

    -- Wrap given object with State.
    wrapState :: a -> State s a
    wrapState x = S {runS = (x :#)}

    -- Construct State by serial application of the inner and outer States annotation functions.
    joinState :: State s (State s a) -> State s a
    joinState state = S {runS = \x -> runS (unAnnotate (runS state x)) (getAnnotation (runS state x)) }

    -- Modify state's annotation function.
    modifyState :: (s -> s) -> State s ()
    modifyState f = S {runS = \x -> () :# f x}

    -- Functor instance of State.
    instance Functor (State s) where
        fmap = mapState

    -- Applicative instance of State.
    instance Applicative (State s) where
        pure = wrapState
        p <*> q = Control.Monad.ap p q

    -- Monad instance of State.
    instance Monad (State s) where
        m >>= f = joinState (fmap f m)

    -- Primitive arithmetic operation's type.
    data Prim a =
          Add a a      -- (+)
        | Sub a a      -- (-)
        | Mul a a      -- (*)
        | Div a a      -- (/)
        | Abs a        -- abs
        | Sgn a deriving Show 

    -- Arithmetic expression's type
    data Expr = Val Double | Op (Prim Expr) deriving Show

    -- Num instance of Expr.
    instance Num Expr where
        x + y = Op (Add x y)
        x * y = Op (Mul x y)
        x - y = Op (Sub x y)
        abs x = Op (Abs x)
        signum x = Op (Sgn x)
        negate x = Op (Mul x (fromInteger (-1)))
        fromInteger x = Val (fromInteger x)

    -- Fractional instance of Expr.
    instance Fractional Expr where
        fromRational x = Val (fromRational x)
        x / y = Op (Div x y)

    -- Evaluate the Expr and return the result value and sequence of calculations. 
    eval :: Expr -> State [Prim Double] Double
    eval (Val x)        = return x
    eval (Op (Add x y)) = evalBiFunc x y Add (+)
    eval (Op (Sub x y)) = evalBiFunc x y Sub (-)
    eval (Op (Mul x y)) = evalBiFunc x y Mul (*)
    eval (Op (Div x y)) = evalBiFunc x y Div (/)
    eval (Op (Abs x))   = evalMonoFunc x Abs abs
    eval (Op (Sgn x))   = evalMonoFunc x Sgn signum

    -- Evaluate the unary operation. Parameters: given expression, Prim constructor and matching function.
    evalMonoFunc :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> State [Prim Double] Double
    evalMonoFunc x cr op =
        do
            resx <- eval x
            modifyState (cr resx :)
            return (op resx)

    -- Evaluate the binary operation. Parameters: given expressions, Prim constructor and matching function.
    evalBiFunc :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> State [Prim Double] Double
    evalBiFunc x y cr op =
        do
            resx <- eval x
            resy <- eval y
            modifyState (cr resx resy :)
            return (op resx resy)
