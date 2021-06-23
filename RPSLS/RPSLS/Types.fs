module RPSLS.Types

type Action =
   | Rock
   | Paper
   | Scissors
   | Lizard
   | Spock
  
type Result =
    | Win
    | Loose
    | Tied

type GameState = GameState of int * int * int