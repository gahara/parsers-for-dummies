import Control.Applicative

type Function a b = a -> b

-- (.) :: (a -> b) -> (b -> c) -> (a -> c)
instance Functor (Function a) where
	fmap f g = f . g

-- same:
-- const :: a -> (b -> a)
-- const :: a ->  b -> a

quick :: Int -> Int -> Int

dick :: Int -> Int
dick 0 = 1
dick x = x + 2

instance Applicative (Function a) where
	-- a -> (b -> a)
	pure = const
	-- (<*>) :: Function a (b -> c) -> Function a b -> Function a c
	hui <*> pizda = \x -> (hui x) (pizda x)

join :: Function a (Function a b) -> Function a b
join f = \x -> (f x) x

instance Monad (Function a) where
	return = pure
	a >>= f = join (fmap f a)