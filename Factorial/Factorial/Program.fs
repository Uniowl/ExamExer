module Factorial

let rec fact = function
      | (0,m) -> m
      | (n,m) -> fact(n-1,n*m);;

[<EntryPoint>]
let main argv =
    printfn $"{fact(5,1)}"
    0