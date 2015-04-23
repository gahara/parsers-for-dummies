type Error = String

data Parser a = Parser { parse :: String -> Either Error (a, String) }
-- data Either a b = Left a | Right b

--parse :: Parser a -> String -> Either Error (a, String)
--parse (Parser a) = a

-- data Dick = Dick Int
-- Dick 4 :: Dick
-- data Dick a = Dick a
-- Dick 4 :: Dick Int
-- data Dick' = Dick Int
-- Dick 4 :: Dick'

--а давайте напишем функцию, которая вернет первую буковку. строка- список символов
--считывает одну букву
anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
    [] -> Left "anyChar: empty input"
    (h:t) -> Right (h,t)

instance Functor Parser where
	fmap f p = Parser $ \s -> case parse p s of --отсюда и ниже это то же самое, что { parse :: String -> Either Error (a, String) }
		Right (a, s') -> Right (f a, s')
		Left e -> Left e


--(<*>) :: f (a -> b) -> f a -> f b
instance Applicative Parser where
	-- a -> 'Parser a
	pure a = Parser $ \s -> Right (a, s)
	p1 <*> p2 = Parser $ \s -> case parse p1 s of
		Left e -> Left e
		Right (a, s') -> case parse p2 s' of
			Left e -> Left e
            Right (b, s'') -> Right (a b, s'')

--(>>=) :: m a -> (a -> m b) -> m b
instance Monad Parser where
	return = pure
    p1 >>= p2 =
    	-- Parser b
    	Parser $
    	-- String -> Either (b, String)
    	\s -> case parse p1 s of
    	Left e -> Left e
    	-- Parser b
    	-- String -> Either Error (b, String)
    	Right (a, s') -> parse (p2 a) s'
    --fail :: Error -> Parser a
    fail e = Parser $ \s -> Left e

--возвращает две буквы
par = anyChar >>= \a -> anyChar >>= \b -> return (a:b:[])
par = do
    a <- anyChar
    b <- anyChar
    return (a:b:[])

--падает, если не то дали. буква, например
satisfies :: (a -> Bool) -> Parser a -> Parser a
satisfies f p = do
    r <- p
    unless (f r) $ fail "satisfies: fail"
    return r

int :: Parser Integer
int = fmap read (some (satisfies isDigit anyChar))

--парсер, который считывает одну букву, и если она не удовлетворяет условию - то все, пиздец
char :: Char -> Parser Char
char c = satisfies (== c) anyChar


sum :: Parser a -> Parser a -> Parser a

4 + (5 + 6) = (4 + 5) + 6

a + (b + c) = (a + b) + c

instance Alternative Parser where
    empty = fail "nikakuyu"
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
        Left e -> parse p2 s
        Right (a, s') -> Right (a, s')


--функция. сначала пробует распарсить первым парсером что-то, если получилось - распаршенную штуку засунуть в голову списка, а в хвосте
-- распарсить остаток и дальше двигаться, пытаясь парсить остаткии. когда не получилось, вернуть пустой список


--TODO альтернативу для парсера
--parse (p2,a) -  -- String -> Either Error (b, String) , ей даем s и получим в итоге Either Error (b, String). 
--когда у функции не хватает аргументов, она возвращает функцию, которая требует остаток. 
-- почему монада без join? а давайте выведем блядский join из ебаного бинда, нам же нечего делать

--(>>=) :: m a -> (a -> m b) -> m b
-- return ::  a -> Parser a
--join ::f (f a) -> f a
--x  -  Parser (Parser a) ( f f a) 
--join x = x >>= id

