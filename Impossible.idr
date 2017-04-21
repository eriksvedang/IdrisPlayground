module Impossible

total
twoAndThreeAreNotEqual : 2 = 3 -> Void
twoAndThreeAreNotEqual Refl impossible



zeroNotSucc : (0 = S k) -> Void
zeroNotSucc Refl impossible

succNotZero : (S k = 0) -> Void
succNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

myCheckEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
myCheckEqNat Z Z = Yes Refl
myCheckEqNat Z (S k) = No zeroNotSucc
myCheckEqNat (S k) Z = No succNotZero
myCheckEqNat (S k) (S j) = case myCheckEqNat k j of
                                (Yes prf) => Yes (cong prf)
                                (No contra) => No (noRec contra)

