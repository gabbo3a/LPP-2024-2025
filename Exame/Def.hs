-- Definire map
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
mmap :: (a -> b) -> [a] -> [b]
mmap _ [] = []
mmap f (x : xs) = f x : mmap f xs 

-- Definire foldr e foldl

-- Definire funzioni su liste

-- length
-- llenght :: [a] -> Int
-- llenght [] = 0
-- llenght (x:xs) = 1 + llenght xs

llenght :: [a] -> Int
llenght = foldl (\n _ -> n + 1) 0

-- sum
-- ssum :: [Int] -> Int
-- ssum [] = 0
-- ssum (x : xs) = x + ssum xs

ssum :: [Int] -> Int
ssum = foldl (\a b -> a + b) 0
    -- product
    -- head, last
    -- take, drop
    -- concat
