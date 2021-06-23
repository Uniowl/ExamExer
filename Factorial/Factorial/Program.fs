// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let rec factA = function
      | (0,m) -> m
      | (n,m) -> factA(n-1,n*m);;
      

let rec fact = function
      | 0 -> 1
      | n -> n * fact(n-1)

[<EntryPoint>]
let main argv =
    printfn $"{factA(5,1)}"
    0 // return an integer exit code