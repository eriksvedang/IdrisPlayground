module Olsson

import Data.Vect

p : (n ** Vect n Int)
p = (5 ** [1,2,3,4,5])

--data Pair a b = MkPair a b

-- data PAIR : (a : Type) -> (P : a -> Type) -> Type where
--      Depp : (x : a) -> (pf : P x) -> DPair a P

data DPair2 : (a : Type) -> (P : a -> Type) -> Type where
     MkDPair2 : {Q : a -> Type} -> (x : a) -> (pf : Q x) -> DPair2 a Q

 
