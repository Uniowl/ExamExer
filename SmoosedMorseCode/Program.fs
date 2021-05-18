// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let smorse str =
    let mutable encoding = ""
    for chr in str do
        match chr with
        | 'a' -> encoding <- encoding + ".-"
        | 'b' -> encoding <- encoding + "-..."
        | 'c' -> encoding <- encoding + "-.-."
        | 'd' -> encoding <- encoding + "-.."
        | 'e' -> encoding <- encoding + "."
        | 'f' -> encoding <- encoding + "..-."
        | 'g' -> encoding <- encoding + "--."
        | 'h' -> encoding <- encoding + "...."
        | 'i' -> encoding <- encoding + ".."
        | 'j' -> encoding <- encoding + ".---"
        | 'k' -> encoding <- encoding + "-.-"
        | 'l' -> encoding <- encoding + ".-.."
        | 'm' -> encoding <- encoding + "--"
        | 'n' -> encoding <- encoding + "-."
        | 'o' -> encoding <- encoding + "---"
        | 'p' -> encoding <- encoding + ".--."
        | 'q' -> encoding <- encoding + "--.-"
        | 'r' -> encoding <- encoding + ".-."
        | 's' -> encoding <- encoding + "..."
        | 't' -> encoding <- encoding + "-"
        | 'u' -> encoding <- encoding + "..-"
        | 'v' -> encoding <- encoding + "...-"
        | 'w' -> encoding <- encoding + ".--"
        | 'x' -> encoding <- encoding + "-..-"
        | 'y' -> encoding <- encoding + "-.--"
        | 'z' -> encoding <- encoding + "--.."
        | _ -> encoding <- encoding
    encoding
    
let readLines filePath = System.IO.File.ReadLines(filePath);;

[<EntryPoint>]
let main argv =
    let words = readLines "D:\Skole\AFP\ExamExer\SmoosedMorseCode\SmoosedMorseCode\enable.txt"
    let count = Seq.countBy smorse words
    let mutable magicWord =""
    for c in count do
        if ((snd c) = 13) then do
            magicWord <- fst c
    
    printfn "%A" magicWord
    
    for word in  (Seq.filter (fun w -> smorse w = magicWord) words) do
        printfn "%A" word
    
    0