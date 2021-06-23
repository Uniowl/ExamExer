open System
open RPSLS.GameLogic_SmartComp
open RPSLS.Types
let run () =
    gameLoop ()
    printf "\nPress any key to exit...\n"
    Console.ReadKey () |> ignore


[<EntryPoint>]
let main argv =
    run ()
    0