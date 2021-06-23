module RPSLS.GameLogic
open System
open RPSLS.Types
let userChoice () =
    let rec loop () =
        printf "\nPlease choose one of the following actions:\nr -> Rock\np -> Paper\ns -> Scissor\nl -> Lizard\nsp -> Spock\nChoice: "
        match Console.ReadLine () with
        | "r"   -> Rock
        | "p"   -> Paper
        | "s"   -> Scissors
        | "l"   -> Lizard
        | "sp"  -> Spock
        | _     ->
            printf "Invalid action! "
            loop ()
    loop ()
    
let compChoice (rnd: Random) =
    match rnd.Next(0,5) with
    | 0   -> Rock
    | 1   -> Paper
    | 2   -> Scissors
    | 3   -> Lizard
    | _   -> Spock
    
let result () =
    let randomizer = Random()
    let cc = compChoice randomizer
    let uc = userChoice ()
    printf $"You picked: \t\t{uc} \nComputer picked: \t{cc}"
    match (uc, cc) with
    | Scissors, Paper  -> ("Scissors cuts Paper",          Win   )    // Scissors cuts Paper                           
    | Paper, Scissors  -> ("Scissors cuts Paper",          Loose )    //                                               
    | Paper, Rock      -> ("Paper covers Rock",            Win   )    // Paper covers Rock                             
    | Rock, Paper      -> ("Paper covers Rock",            Loose )    //                                               
    | Rock, Lizard     -> ("Rock crushes Lizard",          Win   )    // Rock crushes Lizard                           
    | Lizard, Rock     -> ("Rock crushes Lizard",          Loose )    //                                               
    | Lizard, Spock    -> ("Lizard poisons Spock",         Win   )    // Lizard poisons Spock                          
    | Spock, Lizard    -> ("Lizard poisons Spock",         Loose )    //                                               
    | Spock, Scissors  -> ("Spock smashes Scissors",       Win   )    // Spock smashes Scissors                        
    | Scissors, Spock  -> ("Spock smashes Scissors",       Loose )    //                                               
    | Scissors, Lizard -> ("Scissors decapitates Lizard",  Win   )    // Scissors decapitates Lizard                   
    | Lizard, Scissors -> ("Scissors decapitates Lizard",  Loose )    //                                               
    | Lizard, Paper    -> ("Lizard eats Paper",            Win   )    // Lizard eats Paper                             
    | Paper, Lizard    -> ("Lizard eats Paper",            Loose )    //                                               
    | Paper, Spock     -> ("Paper disproves Spock",        Win   )    // Paper disproves Spock                         
    | Spock, Paper     -> ("Paper disproves Spock",        Loose )    //                                               
    | Spock, Rock      -> ("Spock vaporizes Rock",         Win   )    // Spock vaporizes Rock                          
    | Rock, Spock      -> ("Spock vaporizes Rock",         Loose )    //                                               
    | Rock, Scissors   -> ("Rock crushes Scissors",        Win   )    // (and as it always has) Rock crushes Scissors  
    | Scissors, Rock   -> ("Rock crushes Scissors",        Loose )
    | _                ->
        ($"You and the computer both picked {cc}",         Tied  )

let gameRound () =
    let string, result = result ()
    printf $"\n{string}. You {result}!"
    result

let printGameStats (GameState(wins, losses, ties)) =
    let winRate = float wins / float (wins + losses) * 100.0
    printf $"\nYou won %.2f{if wins = 0 then 0.0 else winRate} percent of your {wins+losses+ties} games with a total of {wins} wins, {losses} losses and {ties} ties"

let newGameState (GameState(wins, losses, ties)) =
    let newState =
        match gameRound () with
        | Win   -> GameState(wins+1, losses,   ties)
        | Loose -> GameState(wins,   losses+1, ties)
        | Tied  -> GameState(wins,   losses,   ties+1)
    newState

let gameLoop () =
    let rec recLoop (GameState(wins, losses, ties)) =
        let ngs = newGameState (GameState(wins, losses, ties))
        printf "\nDo you want to play again? (y/n)"
        let rec recContinue () =
            match Console.ReadLine() with
            | "y"   -> // Recursive case
                printf "\nNew Round Started!"
                recLoop ngs
            | "n"   -> // Base case
                printGameStats ngs
            | _     -> // Error case
                printf "Invalid option. Please type \"y\" or \"n\": "
                recContinue ()
        recContinue ()
    recLoop (GameState(0, 0, 0))
    