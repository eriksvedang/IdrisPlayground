-- Insertion sort

import Data.Vect

insert : Ord elem => (x : elem) -> (xsSorted : Vect n elem) -> Vect (S n) elem
insert x [] = [x]
insert x (y :: xs) = if x < y
                     then x :: y :: xs
                     else y :: insert x xs

insSort : Ord a => Vect n a -> Vect n a
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs
                    in  insert x xsSorted

-- Excercise (p 75)

rev : List a -> List a
rev [] = []
rev (x :: xs) = (reverse xs) ++ [x]

len : List a -> Int
len [] = 0
len (x :: xs) = 1 + (len xs)

map : (a -> b) -> Vect n a -> Vect n b
map f [] = []
map f (x :: xs) = f x :: map f xs
