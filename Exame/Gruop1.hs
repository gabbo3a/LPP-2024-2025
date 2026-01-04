
data Tree a = Empty | Node a [Tree a]

-- normalize :: Tree a -> Tree a
-- normalize Empty = Empty
-- normalize (Node x childs) =
--     Node x filter (/= Empty)

-- Domanda 2
filterEvenIndex :: [a] -> [a]
filterEvenIndex xs = map snd fpaired
    where
        paired  = zip [0..] xs
        fpaired = filter (even . fst) paired

filterEvenIndexRec :: [a] -> [a]
filterEvenIndexRec xs = aux xs True
    where
        aux []       take = []
        aux (x : xs) take =
            if take
                then x : aux xs False
                else aux xs True

filterEvenIndexListComp :: [a] -> [a]
filterEvenIndexListComp xs = [x | (i, x) <- zip [0..] xs, even i]

-- Domanda 3
-- numberInv :: Ord a => [a] -> Int
-- numberInv xs = length $ filter id $ zipWith (>) xs (tail xs)

numberInv :: Ord a => [a] -> Int
numberInv xs = length [() | (x, y) <- zip xs (tail xs), x > y]

numberInvRec :: Ord a => [a] -> Int
numberInvRec []             = 0
numberInvRec [x]            = 0
numberInvRec (x : x' : xs)  =
    if x' < x
        then 1 + numberInvRec (x' : xs)
        else numberInvRec (x' : xs)

-- Domanda 4
takeLastEven :: Integral a => [a] -> Maybe a
takeLastEven xs =
    case feven of
        [] -> Nothing
        _ -> Just (head feven)
    where
        feven = reverse (filter even xs)

takeLastEvenRec :: Integral a => [a] -> Maybe a
takeLastEvenRec []       = Nothing
takeLastEvenRec [x]      = Just x
takeLastEvenRec (x : xs) = takeLastEvenRec xs

-- Domanda 5
existsDistinct :: Eq a => [a] -> [a] -> Bool
existsDistinct xs ys = not.null $ [x | x <- xs, x `notElem` ys]

existsDistinctRec :: Eq a => [a] -> [a] -> Bool
existsDistinctRec [] [] = False
existsDistinctRec _ []  = True
existsDistinctRec [] _  = False
existsDistinctRec (x : xs) ys =
    aux x ys || existsDistinctRec xs ys
    where
        aux _ [] = True
        aux x (y:ys) =
            if x == y then False else aux x ys

-- Domanda 8
elements :: Tree a -> [a]
elements Empty       = []
elements (Node x xs) = x : foldl (\a b -> a ++ elements b) [] xs 