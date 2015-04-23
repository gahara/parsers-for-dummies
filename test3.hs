import Prelude hiding (Maybe(..))
import Control.Applicative

data Maybe a = Just a | Nothing

instance Functor Maybe where
	-- fmap :: (a -> b) -> (f a -> f b)
	fmap f Nothing = Nothing
	fmap f (Just a) = Just (f a)

instance Applicative Maybe where
	-- pure :: a -> f a
	-- <*> :: f (a -> b) -> f a -> f b
	pure a = Just a 
	_ <*> Nothing = Nothing
	Nothing <*> _ = Nothing
	(Just a) <*> (Just b) = Just (a b)

join :: Maybe (Maybe a) -> Maybe a
join (Just (Just a)) = Just a
join (Just Nothing) = Nothing
join Nothing = Nothing

instance Monad Maybe where
	-- (>>=) :: m a -> (a -> m b) -> m b
	a >>= f = join (fmap f a) 
	