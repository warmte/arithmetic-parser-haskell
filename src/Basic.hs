module Basic where
    data Option a = None | Some a deriving Show 
    data Pair a = P a a deriving Show
    data Quad a = Q a a a a deriving Show
    data Annotated e a = a :# e deriving Show
    infix 0 :#
    data Except e a = Error e | Success a deriving Show
    data Prioritised a = Low a | Medium a | High a  deriving Show
    data Stream a = a :> Stream a  deriving Show
    infixr 5 :>
    data List a = Nil | a :. List a  deriving Show
    infixr 5 :.
    data Fun i a = F (i -> a) 
    data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

    -- | Apply given function to the value inside 
    -- the given Option and returns Option of a result.
    mapOption      :: (a -> b) -> (Option a -> Option b)
    mapOption f None     = None
    mapOption f (Some x) = Some (f x)

    -- | Apply given function to the values inside 
    -- the given Pair and returns Pair of the results.
    mapPair        :: (a -> b) -> (Pair a -> Pair b)
    mapPair f (P x y) = P (f x) (f y)

    -- | Apply given function to the values inside 
    -- the given Quad and returns Quad of the results.
    mapQuad        :: (a -> b) -> (Quad a -> Quad b)
    mapQuad f (Q x y w z) = Q (f x) (f y) (f w) (f z)

    -- | Apply given function to the value inside the given Annotated 
    -- and returns Annotated of a result with the same note.
    mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
    mapAnnotated f (a :# e) = f a :# e

    -- | Apply given function to the value inside the given Except 
    -- and returns Except of a result with the same error.
    mapExcept      :: (a -> b) -> (Except e a -> Except e b)
    mapExcept f (Error x)   = Error x
    mapExcept f (Success x) = Success (f x)

    -- | Apply given function to the value inside the given Prioritised 
    -- and returns Prioritised of a result with the same priority.
    mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
    mapPrioritised f (Low x)    = Low (f x)
    mapPrioritised f (Medium x) = Medium (f x)
    mapPrioritised f (High x)   = High (f x)

    -- | Apply given function to the values inside the given Stream 
    -- and returns Stream of a results.
    mapStream      :: (a -> b) -> (Stream a -> Stream b)
    mapStream f (x :> y) = f x :> mapStream f y

    -- | Apply given function to the values inside the given List 
    -- and returns List of a results.
    mapList        :: (a -> b) -> (List a -> List b)
    mapList f Nil      = Nil
    mapList f (x :. y) = f x :. mapList f y

    -- | Return composition of the given function and function inside the given Fun.
    mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
    mapFun f (F g) = F (\x -> f (g x))

    -- | Apply given function to the values inside the given Tree 
    -- and returns Tree of a results with the same structure.
    mapTree        :: (a -> b) -> (Tree a -> Tree b)
    mapTree f Leaf                    = Leaf
    mapTree f (Branch left cur right) = Branch (mapTree f left) (f cur) (mapTree f right)
