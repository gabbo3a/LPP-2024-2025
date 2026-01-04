-- Esercizio 11: quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = ys ++ [x] ++ zs
    where
        ys = quicksort (filter (<= x) xs)
        zs = quicksort (filter (> x) xs)
    
-- Esercizio 12: Albero di ricerca binario (BST)
-- Definisci un tipo di dato:
-- data BST a = Vuoto | Nodo a (BST a) (BST a)
-- e scrivi una funzione:
-- inserisci :: Ord a => a -> BST a -> BST a
-- che inserisce un elemento mantenendo l'ordinamento.

-- Esercizio 13: funzione che compone una lista di funzioni
-- Scrivi una funzione:
-- che compone in sequenza tutte le funzioni in una lista.

comp :: [a -> a] -> (a -> a)
comp [] = id
comp (f : fs) = f . comp fs
-- comp = foldr (.) id

-- Esercizio 14: verifica bilanciamento di parentesi
-- Scrivi una funzione:
-- stringa contiene parentesi tonde bilanciate.
bil :: String -> Bool
bil ps = aux ps 0
  where
    aux "" 0 = True
    aux "" _ = False
    aux (p:ps) n
      | n < 0     = False
      | p == '('  = aux ps (n + 1)
      | p == ')'  = aux ps (n - 1)
      | otherwise = aux ps n

-- Esercizio 15: numeri primi
primes :: [Int]
primes = filter (aux) [2..]
    where
        aux n =
            all (\x -> mod n x /= 0) [2..n `div` 2]

-- Esercizio 16: foldl personalizzato
-- Scrivi una funzione:
-- piegaSinistra :: (b -> a -> b) -> b -> [a] -> b
-- che simula il comportamento di 'foldl'.


-- Esercizio 17: Maybe e gestione errori
safediv :: Double -> Double -> Maybe Double
safediv a b =
    if b /= 0 then Just(a / b) else Nothing

-- Esercizio 18: parsing di espressioni semplici
-- che valuta un'Exp aritmetica.
data Exp = 
    Val Int     | 
    Sum Exp Exp |
    Mul Exp Exp

val :: Exp -> Int
val (Val n) = n
val (Sum a b) = val a + val b
val (Mul a b) = val a * val b

-- Esercizio 19: Lista infinita di Fibonacci
fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

-- Esercizio 20: Uso del `State` monad
-- Usa `Control.Monad.State` per scrivere una funzione:
-- numeriContati :: [Int] -> State Int [Int]
-- che restituisce la lista originale ma tiene traccia di quanti numeri sono stati elaborati (nello stato).
