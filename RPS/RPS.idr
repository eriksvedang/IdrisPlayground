module RPS

public export
data Move = Rock | Paper | Scissors

public export
data Winner = Left | Right | Tie

match : Move -> Move -> Winner
match Rock Rock = Tie
match Rock Paper = Left
match Rock Scissors = Right
match Paper Rock = Left
match Paper Paper = Tie
match Paper Scissors = Right
match Scissors Rock = Right
match Scissors Paper = Left
match Scissors Scissors = Tie


