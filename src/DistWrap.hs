module DistWrap where
    import Basic (Option (..), Pair (..), Quad (..), Annotated (..), Except (..), Prioritised (..), Stream (..), List (..), Fun (..))

    -- | Take pair of Option and return the Option containing 
    -- the None if at least one of elements were None and pair of their elements otherwise.
    distOption      :: (Option a, Option b) -> Option (a, b)
    distOption (_, None)        = None
    distOption (None, _)        = None
    distOption (Some x, Some y) = wrapOption (x, y)

    -- | Take pair of Pair and return the Pair containing 
    -- the two pairs of their elements.
    distPair        :: (Pair a, Pair b) -> Pair (a, b)
    distPair (P x y, P w z) = P (x, w) (y, z)

    -- | Take pair of Quad and return the Quad containing 
    -- the four pairs of their elements.
    distQuad        :: (Quad a, Quad b) -> Quad (a, b)
    distQuad (Q x1 y1 w1 z1, Q x2 y2 w2 z2) = Q (x1, x2) (y1, y2) (w1, w2) (z1, z2)

    -- | Take pair of Annotated and return the Annotated containing 
    -- the pair of given elements annotated with <> of their notes.
    distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
    distAnnotated (x1 :# y1, x2 :# y2) = (x1, x2) :# y1 <> y2

    -- | Take pair of Except and return the Except containing 
    -- the pair of each elements if both were Success and first Error in given pair otherwise. 
    distExcept      :: (Except e a, Except e b) -> Except e (a, b)
    distExcept (Success x, Success y) = wrapExcept (x, y)
    distExcept (Error x, Success y)   = Error x
    distExcept (Success x, Error y)   = Error y
    distExcept (Error x, Error y)     = Error x

    -- | Take pair of Prioritised and return the Prioritised containing 
    -- the pair of each elements with the higher of two their priorities. 
    distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
    distPrioritised (Low x, Low y)       = Low (x, y)
    distPrioritised (Low x, Medium y)    = Medium (x, y)
    distPrioritised (Low x, High y)      = High (x, y)
    distPrioritised (Medium x, Low y)    = Medium (x, y)
    distPrioritised (Medium x, Medium y) = Medium (x, y)
    distPrioritised (Medium x, High y)   = High (x, y)
    distPrioritised (High x, Low y)      = High (x, y)
    distPrioritised (High x, Medium y)   = High (x, y)
    distPrioritised (High x, High y)     = High (x, y)

    -- | Take pair of Streams and return the Stream containing 
    -- pairs of elements from first and second Streams.
    distStream      :: (Stream a, Stream b) -> Stream (a, b)
    distStream (x :> xs, y :> ys) = (x, y) :> distStream (xs, ys)

    -- | Take pair of Lists and return the List containing 
    -- the cartesian product of each pair of elements from first and from second list.
    distList        :: (List a, List b) -> List (a, b)
    distList (x, y) = bufDistList x y y

    -- | Take two lists and copy of second one and return the List containing 
    -- the cartesian product of each pair of elements from first and from second list.
    bufDistList :: List a -> List b -> List b -> List (a, b)
    bufDistList Nil lst _               = Nil
    bufDistList (x :. xs) lst Nil       = bufDistList xs lst lst
    bufDistList (x :. xs) lst (y :. ys) = (x, y) :. bufDistList (x :. xs) lst ys

    -- | Take pair of Funs and return the Fun containing the function 
    -- which returns pair of their results.
    distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
    distFun (F x, F y) = F (\w -> (x w, y w))

    -- | Wrap given value with the Some into Option.
    wrapOption      :: a -> Option a
    wrapOption = Some

    -- | Wrap given value into Pair of its four times repetition.
    wrapPair        :: a -> Pair a
    wrapPair x = P x x

    -- | Wrap given value into Quad of its four times repetition.
    wrapQuad        :: a -> Quad a
    wrapQuad x = Q x x x x

    -- | Wrap given value with the empty note into Annotation.
    wrapAnnotated   :: Monoid e => a -> Annotated e a
    wrapAnnotated x = x :# mempty

    -- | Wrap given value with the Success into Except.
    wrapExcept      :: a -> Except e a
    wrapExcept = Success

    -- | Wrap given value with the Low priority into Prioritised.
    wrapPrioritised :: a -> Prioritised a
    wrapPrioritised = Low

    -- | Wrap given value with the Stream of it.
    wrapStream      :: a -> Stream a
    wrapStream x = x :> wrapStream x

    -- | Wrap given value into List with one element.
    wrapList        :: a -> List a
    wrapList x = x :. Nil

    -- | Wrap given value with the function const into Fun.
    wrapFun         :: a -> Fun i a
    wrapFun x = F (const x)
