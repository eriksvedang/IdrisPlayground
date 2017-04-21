module Chapter8

data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

ex1 : Vect 3 Int
ex1 = [1,2,3]

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
     Same : (num : Nat) -> EqNat num num

works : EqNat 5 5
works = Same _

-- Should be called sameSucc, since that's what we're proofing?
sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS k k (Same k) = Same (S k)

-- The explicit arguments k and j are not needed:
sameS' : (eq : EqNat k j) -> EqNat (S k) (S j)
sameS' (Same k) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just eq) => Just (sameS' eq)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 (Just (Same len)) => Just input
                                 
ex2 : "Hejsan" = "Hejsan"
ex2 = the ("Hejsan" = "Hejsan") Refl
                                 
-- ex3 : "Hejsan" = "Hejsan"
-- ex3 = the ("Svejsan" = "Hejsan") Refl

checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat' Z Z = Just Refl
checkEqNat' Z (S k) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) = case checkEqNat' k j of
                               Nothing => Nothing
                               (Just prf) => Just (cong prf) -- cong is a generic version of sameS

-- Exercise 1 (p. 219)

total
sameCons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
sameCons Refl = Refl

-- Exercise 2

total
sameLists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
sameLists Refl Refl = Refl

-- Exercise 3

data ThreeEq : a -> b -> c -> Type where
     Same3 : ThreeEq x x x

ex3 : ThreeEq 3 3 3
ex3 = Same3

total
allSameSucc : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameSucc z z z Same3 = Same3
