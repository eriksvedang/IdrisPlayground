module Elem

import Data.Vect

-- Failed attempt:
-- myRemoveElem : DecEq a => (value : a) -> Vect (S n) a -> Vect n a
-- myRemoveElem value (x :: xs) = case decEq value x of
--                                     (Yes Refl) => xs
--                                     (No contra) =>  --x :: myRemoveElem value xs

data MyElem : (value : a) -> (xs : Vect k a) -> Type where
     MyHere : MyElem x (x :: xs)
     MyThere : (later : MyElem x xs) -> MyElem x (y :: xs)

ex1 : MyElem 10 [10,20,30]
ex1 = MyHere

ex2 : MyElem 20 [10,20,30]
ex2 = MyThere MyHere

ex3 : MyElem 30 [10,20,30]
ex3 = MyThere (MyThere MyHere)



--- These doesn't use MyElem because I can't figure out how to implement Uninhabited for it...

-- My removeElem function
myRemoveElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
myRemoveElem value (value :: ys) Here = ys
myRemoveElem {n = Z} value (y :: []) (There later) = absurd later
myRemoveElem {n = (S k)} value (y :: ys) (There later) = y :: myRemoveElem value ys later

myRemoveElem_auto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
myRemoveElem_auto value xs {prf} = myRemoveElem value xs prf


-- My isElem function
notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (x = value) -> Void) ->
            (notThere : Elem value xs -> Void) ->
            Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

myIsElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
myIsElem value [] = No notInNil
myIsElem value (x :: xs) = case decEq x value of
                                (Yes Refl) => Yes Here
                                (No notHere) => (case myIsElem value xs of
                                                      (Yes prf) => Yes (There prf)
                                                      (No notThere) => No (notInTail notHere notThere))
