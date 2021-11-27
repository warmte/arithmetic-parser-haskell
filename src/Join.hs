module Join where
    import Basic (Option (..), Annotated (..), Except (..), List (..), Fun (..))

    -- Moves value from inner Option to the outer one.
    joinOption    :: Option (Option a) -> Option a
    joinOption None     = None
    joinOption (Some x) = x

    -- Moves value from inner Except to the outer one.
    joinExcept    :: Except e (Except e a) -> Except e a
    joinExcept (Error x)   = Error x
    joinExcept (Success x) = x

    -- Moves value from inner Option to the outer one and set <> of inner and outer notes to note.
    joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
    joinAnnotated ((x :# z) :# y) = x :# (y <> z)

    -- Moves values from all the inner lists to the one.
    joinList      :: List (List a) -> List a
    joinList Nil = Nil
    joinList (x :. xs) = revList Nil (mergeLists (revList x Nil) (joinList xs))
        where

        mergeLists    :: List a -> List a -> List a
        mergeLists Nil y       = y
        mergeLists (x :. xs) y = mergeLists xs (x :. y)

        revList       :: List a -> List a -> List a
        revList Nil res       = res
        revList (x :. xs) res = revList xs (x :. res)

    -- Applies function from the inner Fun to the outer function's argument.
    joinFun       :: Fun i (Fun i a) -> Fun i a
    joinFun (F f) = F (\w -> applyFun (f w) w)
        where

        applyFun     :: Fun i a -> i -> a
        applyFun (F f) = f
