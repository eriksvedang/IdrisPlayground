module Records

record Person where
       constructor MkPerson
       name : String
       age : Nat
       
record Album where
       constructor MkAlbum
       name : String
       price : Double

me : Person       
me = MkPerson "Erik" 30

some : Person
some = record { name = "Someone", age = 100 } (MkPerson "" 0)

sgtPepper : Album
sgtPepper = MkAlbum "Sgt Pepper's Lonely Hearts Club Band" 29.90

record Changeling a where
       constructor MkChangeling
       stuff : a
       
ch : Changeling Int
ch = MkChangeling 100

ch2 : Changeling Int
ch2 = record { stuff $= (*2) } ch

changer : Changeling Int -> Changeling String
changer c = MkChangeling (show (stuff c))
 
ch3 : Changeling String
ch3 = changer ch2

data Hide : Type where
     MkHide : (Num a, Show a) => a -> Hide
     
     
     
hidden : Hide
hidden = MkHide 100     

hidden' : Hide
hidden' = case hidden of
               (MkHide a) => MkHide (a * 2)

-- hid2 : Hide
-- hid2 = MkHide True
