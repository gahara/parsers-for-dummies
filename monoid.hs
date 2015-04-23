import Data.Monoid

data Nums = Zero | One | Two

instance Monoid Nums where
    mempty = Zero

    Zero `mappend` a = a
    a `mappend` Zero = a
    One `mappend` Two = Two
    Two `mappend` One = Two
    One `mappend` One = Two
    Two `mappend` Two = Two

data Maybe a = Just a | Nothing

instance Monoid (Maybe Int) where
    mempty = Nothing

    Nothing `mappend` a = a
    a `mappend` Nothing = a
    (Just a) `mappend` (Just b) = Just a

