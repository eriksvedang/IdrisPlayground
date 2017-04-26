module Door

data DoorState = DoorClosed | DoorOpen

data DoorCmd : Type -> DoorState -> DoorState -> Type where
     Open : DoorCmd     () DoorClosed DoorOpen
     Close : DoorCmd    () DoorOpen DoorClosed
     RingBell : DoorCmd () DoorClosed DoorClosed
     
     Pure : ty -> DoorCmd ty state state
     (>>=) : DoorCmd a s1 s2 -> (a -> DoorCmd b s2 s3) -> DoorCmd b s1 s3

doorProg : DoorCmd () DoorOpen DoorClosed
doorProg = do Close 
              RingBell
              Open
              --Open
              Close

              
