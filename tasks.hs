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
f3 :: [a] -> Integer -> a
f3 [] n = error "vot pidoras"
f3 (a:t) 1 = a 
f3 (a:t) n = f3 t (n - 1)

--количество элементов в списке
f4 :: [a] -> Integer
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
f8 :: [a] -> [a]
f8 [] = []
f8 (a:t) = if foldr (||) False (map (a ==) t) then f8 t else a : f8 t -- или any (map (a ==) t) то же самое

f8' :: [a] -> [a]
f8' f l = f [] l
  where f a [] = a
        f a b 