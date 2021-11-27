module ExceptEvaluator where
    import Basic (Annotated (..), Except(..))
    import SimpleEvaluator (Prim (..), Expr(..))
    import qualified Control.Monad
    import Control.Monad (when)

    -- ExceptState monad with annotation function. Returns either annotated value or an error.
    data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

    -- | Construct ExceptState with the annotation function which modifies 
    -- the given object using the given function.
    mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
    mapExceptState f eState = ES { runES = mapAnnotatedExcept f . runES eState }

    -- Apply given function to the given Except and do nothing if Except is Error.
    mapAnnotatedExcept :: (a -> b) -> Except e (Annotated s a) -> Except e (Annotated s b)
    mapAnnotatedExcept f (Error x)           = Error x
    mapAnnotatedExcept f (Success (x :# xs)) = Success (f x :# xs)

    -- Wrap given object with succesfull ExceptState.
    wrapExceptState :: a -> ExceptState e s a
    wrapExceptState x = ES { runES = \y -> Success (x :# y) }

    -- Construct ExceptState by serial application of the inner and outer States annotation functions.
    joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
    joinExceptState eState = ES { runES = formatEA . runES eState }
        where 
            formatEA :: Except e (Annotated s (ExceptState e s a)) -> Except e (Annotated s a)
            formatEA (Error x)           = Error x
            formatEA (Success (x :# xs)) = runES x xs

    -- Modify state's annotation function.
    modifyExceptState :: (s -> s) -> ExceptState e s ()
    modifyExceptState f = ES { runES = \x -> Success (() :# f x)}

    -- Throws a given error.
    throwExceptState :: e -> ExceptState e s a
    throwExceptState ex = ES { runES = \x -> Error ex }

    -- Functor instance of ExceptState.
    instance Functor (ExceptState e s) where
        fmap = mapExceptState

    -- Applicative instance of ExceptState.
    instance Applicative (ExceptState e s) where
        pure = wrapExceptState
        p <*> q = Control.Monad.ap p q

    -- Monad instance of ExceptState.
    instance Monad (ExceptState e s) where
        m >>= f = joinExceptState (fmap f m)

    data EvaluationError = DivideByZero -- Type of error which could be thrown by the evaluator.

    -- Evaluate the Expr and return the result value and sequence of calculations or DivideByZero error.
    eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
    eval (Val x)        = return x
    eval (Op (Add x y)) = evalBiFunc x y Add (+) False
    eval (Op (Sub x y)) = evalBiFunc x y Sub (-) False
    eval (Op (Mul x y)) = evalBiFunc x y Mul (*) False
    eval (Op (Div x y)) = evalBiFunc x y Div (/) True
    eval (Op (Abs x))   = evalMonoFunc x Abs abs
    eval (Op (Sgn x))   = evalMonoFunc x Sgn signum

    -- Evaluate the unary operation. Parameters: given expression, Prim constructor and matching function.
    evalMonoFunc :: Expr -> (Double  -> Prim Double) -> (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
    evalMonoFunc x cr op =
        do
            resx <- eval x
            modifyExceptState (cr resx :)
            return (op resx)

    -- Check if given Except is an Error.
    isError :: Except e a -> Bool
    isError (Error x) = True
    isError _         = False

    -- Check if given ExceptState returns an error.
    isESError :: ExceptState e s a -> s -> Bool
    isESError x st = isError (runES x st)

    -- Evaluate the binary operation. Parameters: given expressions, Prim constructor and matching function.
    evalBiFunc :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Bool -> ExceptState EvaluationError [Prim Double] Double
    evalBiFunc x y cr op _ | isESError (eval x) [] || isESError (eval y) [] = throwExceptState DivideByZero
    evalBiFunc x y cr op f =
        do
            resx <- eval x
            resy <- eval y
            when (f && resy == 0) (throwExceptState DivideByZero)
            modifyExceptState (cr resx resy :)
            return (op resx resy)
