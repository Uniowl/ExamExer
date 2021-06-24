module RPSLS.GameLogic_SmartComp

open System
open RPSLS.Types

type MoveMemory = MoveMemory of (Action * int) list

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
    
let getMostCommonActions (MoveMemory(mm)) =
    let largestValue = mm |> Seq.ofList |> Seq.map snd |> Seq.sortDescending |> Seq.head
    mm |> List.filter (fun x -> snd x = largestValue) |> List.map fst

let getCounterActions action =
    match action with
    | Rock     -> [Spock; Paper]
    | Paper    -> [Lizard; Scissors]
    | Scissors -> [Rock; Spock]
    | Lizard   -> [Scissors; Rock]
    | Spock    -> [Paper; Lizard]
    
let getCounterActionList mostCommonActions =
    let rec getCounterActionsRec commonActions counterActions =
        match List.length commonActions with
        | 1 -> counterActions @ (getCounterActions commonActions.[0])
        | _ -> getCounterActionsRec (List.splitAt 1 commonActions |> snd) (counterActions @ (getCounterActions commonActions.[0]))
    getCounterActionsRec mostCommonActions []
    
let compChoiceSmart (rnd: Random) mm  =
    let MostCommonActions = getMostCommonActions mm
    let allCounterActions = getCounterActionList  MostCommonActions
    let bestCounterActions = getMostCommonActions (MoveMemory(List.countBy (fun x -> x) allCounterActions))
    let compChoice = rnd.Next(0,(List.length bestCounterActions))
    //This print will help the player:
    printf $"\n\"I SEE YOU LIKE TO USE {MostCommonActions}. MAYBE I SHOULD PICK {bestCounterActions}\""
    bestCounterActions.[compChoice]
    
    
    
let result (mm:MoveMemory) =
    let randomizer = Random()
    let cc = compChoiceSmart randomizer mm
    let uc = userChoice ()
    printf $"You picked: \t\t{uc} \nComputer picked: \t{cc}"
    match (uc, cc) with
    | Scissors, Paper  -> ("Scissors cuts Paper",          Win,    uc)    // Scissors cuts Paper                           
    | Paper, Scissors  -> ("Scissors cuts Paper",          Loose,  uc)    //                                               
    | Paper, Rock      -> ("Paper covers Rock",            Win,    uc)    // Paper covers Rock                             
    | Rock, Paper      -> ("Paper covers Rock",            Loose,  uc)    //                                               
    | Rock, Lizard     -> ("Rock crushes Lizard",          Win,    uc)    // Rock crushes Lizard                           
    | Lizard, Rock     -> ("Rock crushes Lizard",          Loose,  uc)    //                                               
    | Lizard, Spock    -> ("Lizard poisons Spock",         Win,    uc)    // Lizard poisons Spock                          
    | Spock, Lizard    -> ("Lizard poisons Spock",         Loose,  uc)    //                                               
    | Spock, Scissors  -> ("Spock smashes Scissors",       Win,    uc)    // Spock smashes Scissors                        
    | Scissors, Spock  -> ("Spock smashes Scissors",       Loose,  uc)    //                                               
    | Scissors, Lizard -> ("Scissors decapitates Lizard",  Win,    uc)    // Scissors decapitates Lizard                   
    | Lizard, Scissors -> ("Scissors decapitates Lizard",  Loose,  uc)    //                                               
    | Lizard, Paper    -> ("Lizard eats Paper",            Win,    uc)    // Lizard eats Paper                             
    | Paper, Lizard    -> ("Lizard eats Paper",            Loose,  uc)    //                                               
    | Paper, Spock     -> ("Paper disproves Spock",        Win,    uc)    // Paper disproves Spock                         
    | Spock, Paper     -> ("Paper disproves Spock",        Loose,  uc)    //                                               
    | Spock, Rock      -> ("Spock vaporizes Rock",         Win,    uc)    // Spock vaporizes Rock                          
    | Rock, Spock      -> ("Spock vaporizes Rock",         Loose,  uc)    //                                               
    | Rock, Scissors   -> ("Rock crushes Scissors",        Win,    uc)    // (and as it always has) Rock crushes Scissors  
    | Scissors, Rock   -> ("Rock crushes Scissors",        Loose,  uc)
    | _                ->
        ($"You and the computer both picked {cc}",         Tied,   uc)

let gameRound mm =
    let string, result, choice = result mm
    printf $"\n{string}. You {result}!"
    result, choice

let printGameStats (GameState(wins, losses, ties)) (mm:MoveMemory) =
    let winRate = float wins / float (wins + losses) * 100.0
    printf $"\nYou won {if wins = 0 then 0.0 else winRate} percent of your {wins+losses+ties} games with a total of {wins} wins, {losses} losses and {ties} ties"
    

let newGameState (GameState(wins, losses, ties)) (mm:MoveMemory) =
    match (gameRound mm) with
    | Win,  c -> GameState(wins+1, losses, ties), c
    | Loose,c -> GameState(wins, losses+1, ties), c
    | Tied, c -> GameState(wins, losses, ties+1), c

let addToMemory choice (MoveMemory(moveMemory)) =
    MoveMemory(List.map (fun x ->
                                if fst x = choice
                                then (choice, (snd x + 1))
                                else x
                            ) moveMemory)

let gameLoop () =
    let rec recLoop (GameState(wins, losses, ties)) (mm:MoveMemory) =
        let currentGameState, userChoice = newGameState (GameState(wins, losses, ties)) mm
        let newMemory = addToMemory userChoice mm
        printf "\nDo you want to play again? (y/n)"
        let rec recContinue () =
            match Console.ReadLine() with
            | "y"   -> // Recursive case
                printf "\nNew Round Started!"
                recLoop currentGameState newMemory
            | "n"   -> // Base case
                printGameStats currentGameState newMemory
            | _     -> // Error case
                printf "Invalid option. Please type \"y\" or \"n\": "
                recContinue ()
        recContinue ()
    recLoop (GameState(0, 0, 0)) (MoveMemory([Rock, 0; Paper, 0; Scissors, 0; Lizard, 0; Spock, 0]))