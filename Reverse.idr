module Reverse

import Data.Vect

-- Reversing a vector

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} result = rewrite plusCommutative 1 k in result
    
    
-- Exercise 1 (p. 227)

total
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = ?w00t


-- Exercise 2

prf_nil : Vect n a -> Vect (plus n 0) a
prf_nil {n} xs = rewrite plusZeroRightNeutral n in xs

prf_xs : Vect ((S n) + len) a -> Vect (plus n (S len)) a
prf_xs {n} {len} xs = ?dontUnderstandThisYet

total
myReverseFast : Vect n a -> Vect n a
myReverseFast xs = reverse' [] xs
              where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
                    reverse' acc [] = prf_nil acc
                    reverse' acc (x :: xs) = prf_xs (reverse' (x::acc) xs)
