import Control.Applicative

type Error = String

newtype Parser a = Parser { parse :: String -> Either Error (a, String) }

instance Functor Parser where
	-- fmap :: (a -> b) -> Parser a -> Parser b
	fmap f p = Parser $ \s -> case (parse p) s of
		Left e -> Left e
		Right (a, r) -> Right (f a, r)

parseChar :: Parser Char
parseChar = Parser $ \s -> case s of
	"" -> Left "NAHUI"
	h:t -> Right (h, t)



fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

divide :: Float -> Float -> Maybe Float
divide _ 0 = Nothing
divide a b = Just (a / b)

bljad :: Maybe (a -> b) -> Maybe a -> Maybe b


instance Applicative Parser where
	-- a -> Parser a
	pure a = Parser $ \s -> Right (a, s)
	-- Parser (a -> b) -> Parser a -> Parser b
	(<*>) a b = Parser $ \s -> case parse a b

-- data Organ = Penis Int | Vagina Float

-- "penis 4"
-- organ :: Parser (Int -> Organ)
-- organ "penis ":t = Right (\i -> Penis i, t)
-- organ _ = Left "NAHUI"

-- next :: Parser Int

-- (<*>) organ next :: Parser Penis
