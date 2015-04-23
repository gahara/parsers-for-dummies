--маленький язык программирования, который умеет только печатать
import Control.Applicative
import Control.Monad

data Writer a = Writer a String
              deriving (Show, Eq)

instance Functor Writer where
  fmap f (Writer a str) = Writer (f a) str

instance Monad Writer where
  return a = Writer a ""
  (Writer a str) >>= f = let Writer b str' = f a
                        in Writer b (str ++ str')

instance Applicative Writer where
  pure = return
  (<*>) = ap

print' :: String -> Writer ()
print' str = Writer () str

printLn' :: String -> Writer ()
printLn' str = print' (str ++ "\n")

eval :: Writer a -> IO a
eval (Writer a str) = do
  putStr str
  return a

fib :: Integer -> Writer Integer
fib n = do
  printLn' (show n)
  fib' n

  where fib' 0 = return 0
        fib' 1 = return 1
        fib' n = do
          a <- fib (n - 1)
          b <- fib (n - 2)
          return (a + b)