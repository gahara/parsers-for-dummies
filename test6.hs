data List a = Empty | Cons a (List a)
              deriving (Show)

instance Functor List where
	fmap f Empty = Empty
	fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Alternative List where
	empty = Empty
	Empty <|> b = b
	(Cons a t1) <|> b = Cons a (t1 <|> b)

instance Applicative List where
	pure = Cons (a Empty)
	-- <*> :: f (a -> b) -> f a -> f b 
	Empty <*> _ = Empty
	(Cons a t1) <*> b = (fmap a b) <|> (t1 <*> b)

join :: List (List a) -> List a
join Empty = Empty
join (Cons a t0) = a <|> join t0 

instance Monad List where
	return = pure
	a >>= f = join (fmap f a)
--аппликатив - функтор, у которого еще внутри функции друг к другу применять
-- >>= bind 
-- монада - функтор который может из вложенного сделать невложенный. вложенные списки вытянуть в один большой

