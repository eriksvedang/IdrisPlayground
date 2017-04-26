module Stack

import Data.Vect

data StackAction : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackAction () n (S n)
     Pop : StackAction Integer (S n) n
     Pure : a -> StackAction a n n
     (>>=) : StackAction a n1 n2 -> (a -> StackAction b n2 n3) -> StackAction b n1 n3

evalStack : Vect n Integer -> StackAction a n m -> (a, Vect m Integer)
evalStack xs (Push x) = ((), x :: xs)
evalStack (x :: xs) Pop = (x, xs)
evalStack xs (x >>= f) = let (result, newStack) = evalStack xs x
                         in  evalStack newStack (f result)

prog1 : StackAction Integer 0 2
prog1 = do Push 10
           Push 20
           x <- Pop
           Push 30
           Push (x * 100)
           Pop

ex1 : (Integer, Vect 2 Integer)
ex1 = evalStack [] prog1

prog2 : StackAction Integer 1 0
prog2 = do Push 5
           x <- Pop
           y <- Pop
           Pure (x * y)

ex2 : (Integer, Vect 0 Integer)
ex2 = evalStack [6] prog2
