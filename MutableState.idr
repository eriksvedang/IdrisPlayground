module MutableState

data State : (stateType : Type) -> (ty : Type) -> Type where
     Get : State s s
     Put : s -> State s ()
     Pure : a -> State s a
     Bind : State s a -> (a -> State s b) -> State s b
     
(>>=) : State s a -> (a -> State s b) -> State s b
(>>=) = Bind

runState : State s a -> s -> (a, s)
runState Get s = (s, s)
runState (Put newState) _ = ((), newState)
runState (Pure x) s = (x, s)
runState (Bind cmd prog) s = let (result, nextState) = runState cmd s
                             in  runState (prog result) nextState

prog : State Int String
prog = do x <- Get
          Put (10 * x)
          y <- Get
          Put 666
          Pure (show y)
                                
ex1 : (String, Int)
ex1 = runState prog 5
