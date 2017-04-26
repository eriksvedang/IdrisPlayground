module Robot

data Dir = N | W | S | E

Pos : Type
Pos = (Int, Int)

data Move : Type where
     Turn : Dir -> Move
     Forward : Move
     Bind : Move -> ((Pos, Dir) -> Move) -> Move
     
(>>=) : Move -> ((Pos, Dir) -> Move) -> Move
(>>=) = Bind

prog : Move
prog = do Forward
          Turn E
          Forward
          Forward

eval : (Pos, Dir) -> Move -> (Pos, Dir)
eval (pos, dir) (Turn dir') = (pos, dir')
eval ((x, y), N) Forward = ((x, y + 1), N)
eval ((x, y), W) Forward = ((x - 1, y), W)
eval ((x, y), S) Forward = ((x - 1, y), S)
eval ((x, y), E) Forward = ((x + 1, y), E)
eval s (Bind move f) = let s' = eval s move
                       in  eval s' (f s')

ex1 : (Pos, Dir)
ex1 = ((2, 1), E)
