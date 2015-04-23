import Control.Applicative

data List a = Empty | Cons a (List a)

instance Functor List where
	-- fmap :: (a -> b) -> [a] -> [b] 
	fmap f Empty = Empty
	fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
	--pure :: a -> [a]
	pure a = Cons a Empty

	-- (<*>) :: [a -> b] -> [a] -> [b]
	_ <*> Empty = Empty
	Empty <*> _ = Empty
	(Cons a Empty) <*> (Cons b Empty) = Cons (a b) Empty
	(Cons a t) <*> (Cons b t2) = Cons (a b) (t <*> t2)

instance Alternative List where
	empty = Empty
	Empty <|> _ = Empty
	(Cons a t) <|> b =  (Cons a (t <|> b))

join :: List (List a) -> List a
join Empty = Empty
join (Cons a b) = a <|> join b 

instance Monad List where
	return = pure
    a >>= f = join (fmap f a)



