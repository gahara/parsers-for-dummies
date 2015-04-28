import Control.Monad.Random

--Коля мудак1
--найти последний элемент списка
f1 :: [a] -> a
f1 [] = error "dick"
f1 [a] = a
f1 (a:t) = f1 t

--предпоследний
f2 :: [a] -> a
f2 [] = error "mudak"
f2 [a] = error "sovsem mudak"
f2 [a, b] = a
f2 (a:t) = f2 t

--k-й элемент
f3 :: Int -> [a] -> a
f3 n [] = error "vot pidoras"
f3 1 (a:t) = a
f3 n (a:t) = f3 (n - 1) t

--количество элементов в списке
f4 :: [a] -> Int
f4 [] = 0
f4 (a:t) = f4 t + 1

--перевернуть список
f5 :: [a] -> [a]
f5 [] = []
f5 (a:t) = f5 t ++ [a] --сложить два списка
--оно медленное, потом сделаем быстрее

--проверить,является ли списко палиндром
f6' :: Eq a => [a] -> Bool --eq делается для того, чтобы а было сравниваемое
f6' a = reverse a == a

f6 :: Eq a => [a] -> Bool
f6 [] = True
f6 [_] = True
f6 a = head a == last a && f6 (tail $ init a) --tail отбрасывает первый элемент, init - последний

--сделать из списка со вложенными списками один список
f7 :: [[a]] -> [a]
f7 [] = []
f7 (a:t) = a ++ f7 t 

--убрать дубликаты из списка
f8 :: Eq a => [a] -> [a]
f8 [] = []
f8 (a:t)
  | foldr (||) False (map (== a) t) = f8 t
  | otherwise = a : f8 t -- или any (map (a ==) t) то же самое

f8' :: Eq a => [a] -> [a]
f8' l = f [] l
  where f clear [] = clear
        f clear (a:t)
          | foldr (||) False (map (== a) clear) = f clear t
          | otherwise = f (a:clear) t

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (h:t) = qsort [ x | x <- t, x <= h ] ++ [h] ++ qsort [ x | x <- t, x > h ]

--список. в нем есть дубликаты. подряд идущие дубликаты упаковать в список
--[a, a, b, a, a, b, b, c] -> [ 'aa', 'b', 'aa', 'bb', 'c']
f9 :: Eq a => [a] -> [[a]]
f9 a = f [] a
  where f a [] = [a]
        f [] (h:t) = f [h] t
        f a@(h':_) (h:t)
          | h == h' = f (h:a) t
          | otherwise = a : f [] t

f10 :: Eq a => [a] -> [(Int, a)] 
f10 = map (\(h:t) -> (length t + 1, h)) . f9

-- map :: (a -> b) -> [a] -> [b]

-- (a -> b) -> c
-- a -> b -> c

-- a -> (b -> c)
-- a -> b -> c

--[a] -> [Either a (Int, a)] если элемент встретился 1 раз, то писать только его, иначе ...

f11 :: Eq a => [a] -> [Either a (Int, a)]
f11 = map f . f10
  where f (1, a) = Left a
        f pair = Right pair

-- раскодировать f11 назад в полную строчку
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- replicate :: Int -> a -> [a]
f12 :: [(Int, a)] -> [a]
f12 = concat . map (uncurry replicate) -- concatMap

--написать 11 без 10 и 9 и без промежуточных внутренних списков
f13 :: Eq a => [a] -> [Either a (Int, a)]  -- и так понятно встретился
f13 = f11

--взять список и повторить каждый элемент
f14 :: [a] -> [a]
f14 [] = []
f14 (h:t) = h:h:f14 t

-- f14 = f15 2
f15 :: Int -> [a] -> [a]
f15 n [] = []
f15 n (h:t) = replicate n h ++ f15 n t

-- выкинуть каждый n-й элемент
f16 :: Int -> [a] -> [a]
f16 n s = f n s
  where f _ [] = []
        f 1 (h:t) = f n t 
        f n' (h:t) = h : f (n' - 1) t

--разделить список на 2 части : первые n и все остальные
f17 :: Int -> [a] -> ([a], [a])
f17 _ [] = ([], [])
f17 0 a = ([], a)
f17 n (h:t) = let (a, b) = f17 (n - 1) t
              in (h:a, b)

--два числа даются, надо вытащить все элементы между первым и вторым включительно считая с 1
f18 :: Int -> Int -> [a] -> [a]
f18 _ _ [] = []
f18 n m _ | n >= m = []
f18 1 n (h:t) = h : f18 1 (n - 1) t
f18 n m (h:t) = f18 (n - 1) (m - 1) t

-- [1,2,3,4] << 2 === [3,4,1,2]
f19 :: Int -> [a] -> [a]
f19 _ [] = []
f19 0 l = l
f19 n (h:t) = f19 (n - 1) $ t ++ [h]

--удалить k-й
f20 :: Int -> [a] -> [a]
f20 _ [] = []
f20 1 (h:t) = t
f20 k (h:t) = h : f20 (k - 1) t

--вставить на место k
f21 :: Int -> a -> [a] -> [a]
f21 i  a [] = [a]
f21 1 h t = h:t
f21 n i (h:t) = h : f21 (n - 1) i t

--для любых перечислимых n и k делает список от n до k
f22 :: (Ord a, Enum a) => a -> a -> [a]
f22 n m
  | n > m = []
  | otherwise = n : f22 (succ n) m

-- rnd :: Isemechko -> (Float, Isemechko)
--дан список, взять из него n случайных элементов
f23 :: RandomGen g => g -> Int -> [a] -> ([a], g)
f23 g 0 a = (a, g)
f23 g n a = let (i, g') = randomR (0, length a - 1) g
                (es, g'') = f23 g' (n - 1) a
             in (a !! i : es, g'')

f23' :: RandomGen g => Int -> [a] -> Rand g [a]
f23' 0 a = return a
f23' n a = do
  i <- getRandomR (0, length a - 1)
  es <- f23' (n - 1) a
  return $ a !! i : es

--вернуть список из n разных лучайных числ от 1 до m
f24 :: RandomGen g => Int -> Int -> Rand g [Int]
f24 n m = f n [1..m] 
  where f 0 _ = return []
        f _ [] = fail "hui tebe"
        f n a = do
          i <- getRandomR (1, length a)
          es <- f (n - 1) (f20 i a)
          return $ f3 i a : es