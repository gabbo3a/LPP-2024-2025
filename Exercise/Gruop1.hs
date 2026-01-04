-- Esercizio 1: sommaLista
-- somma di tutti gli elementi di una lista.
-- sum1 :: Num a => [a] -> a
-- sum1 [] = 0
-- sum1 (x : xs) = x + sum1 (xs)
-- sum = foldl (+) 0


-- Esercizio 2: fattoriale
-- (es. fattoriale 5 = 120).
fact :: Int -> Int
fact 0 = 1
fact 1 = 1
{-- fact n = n * fact n'
    where 
        n' = n - 1 --}
-- fact n = n * fact (n - 1)
-- fact n = foldr (*) 1 [1..n]
fact n = product [1..n]

-- Esercizio 3: lunghezza
-- senza usare 'length'.
leng :: [a] -> Int
-- leng [] = 0
-- leng (x : xs) = 1 + leng xs
-- leng xs = foldr (\ x -> (+) 1) 0 xs
leng = foldr (\ x -> (+) 1) 0

-- Esercizio 4: inverti
rev :: [a] -> [a]
rev xs = aux xs []
    where
        aux []       acc = acc
        aux (x : xs) acc = aux xs (x: acc)

-- Esercizio 5: appartiene
app :: Eq a => a -> [a] -> Bool
-- app x' [] = False
-- app x' (x : xs) =
--     if x' == x then True else app x' xs
app x' (x : xs) = (x == x') || app x' xs

-- Esercizio 6: mappa
-- Scrivi una versione personalizzata della funzione 'map':
mmap :: (a -> b) -> [a] -> [b]
mmap f [] = []
mmap f (x : xs) = (f x) : (mmap f xs)

-- Esercizio 7: fibonacci
-- che calcola l’n-esimo numero della sequenza di Fibonacci.
-- fib :: Int -> Int

-- Esercizio 8: filtraPari

-- che filtra e restituisce solo i numeri pari da una lista.
pair :: [Int] -> [Int]
-- pair []       = []
-- pair (x : xs) = tmp ++ pair xs
    -- where tmp = if mod x 2 == 0 then [x] else []
pair xs = [x | x <- xs, mod x 2 == 0]

-- Esercizio 9: Albero e profondità
data Tree a = Leaf a | Node (Tree a) (Tree a)
depth :: Tree a -> Int
depth (Leaf x)   = 0
depth (Node x y) = 1 + max (depth x) (depth y)

-- Esercizio 10: conteggio
-- volte un elemento appare in una lista.
-- count :: Eq a => a -> [a] -> Int
-- count x' []       = 0
-- count x' (x : xs) = n + count x' xs
--     where n = if x' == x then 1 else 0

count x' xs = 
    foldr (\x acc -> if x' == x then 1 + acc else 0) 0 xs