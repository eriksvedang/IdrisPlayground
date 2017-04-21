module Chapter4

-- My own Vect data type
data Vect : Nat -> Type -> Type where
     Nil : Vect 0 a
     (::) : (x : a) -> (xs : Vect n a) -> Vect (S n) a

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] y = y
append (x :: xs) y = x :: append xs y

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys


