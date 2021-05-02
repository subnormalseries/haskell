module Chapter17 where

-- applicative functors are monoidal functors
{-

-}
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    mappend (a, b) (a', b') = (,) (a <> a') (b <> b')

instance (Monoid a) => Applicative ((,) a) where
    pure x = (mempty, x)
    (x, f) <*> (x', y) =  (x <> x', f y)


instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend Nothing _ = Nothing
    mappend _ Nothing = Nothing
    mappend (Just x) (Just y) = Just (x <> y)

instance Applicative Maybe where
    pure = Just
    (<*>) (Just f) (Just x) = Just (f x)
    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing

instance Applicative [] where
     pure x = [] x
     (<*>) [] x = []
     (<*>) ([] f) x = ([] f x) -- This is probably not correct



