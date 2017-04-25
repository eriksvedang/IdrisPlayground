module Play

import Data.Vect

tf : Type -> Nat -> Type
tf t n = if n == 0 then Bool else t

f : Vect n a -> tf a n
f [] = False
f (x :: xs) = x

empty : Vect 0 Int
empty = []

p : 1 + 1 = 2
p = Refl

data T : Type where
     A : a -> T

data S = B a
