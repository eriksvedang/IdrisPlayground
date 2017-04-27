module HangmanVerified

import Data.Vect

%default total

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GuessResult = Correct | Incorrect

data GameState : Type where
     NotRunning : GameState
     Running : (guesses : Nat) -> (letters : Nat) -> GameState
     
data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (letters word))))
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S guesses)) (const NotRunning)
     Guess : (c : Char) -> GameCmd GuessResult (Running (S guesses) (S letters))
                                   (\res => case res of
                                                 Correct => Running (S guesses) letters
                                                 Incorrect => Running guesses (S letters))
     
     Pure : (res : ty) -> GameCmd ty (state_fn res) state_fn
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn

