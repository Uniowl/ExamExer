// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Collections

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

let charEncoder chr =
    match chr with
        | 'a' -> ".-"
        | 'b' -> "-..."
        | 'c' -> "-.-."
        | 'd' -> "-.."
        | 'e' -> "."
        | 'f' -> "..-."
        | 'g' -> "--."
        | 'h' -> "...."
        | 'i' -> ".."
        | 'j' -> ".---"
        | 'k' -> "-.-"
        | 'l' -> ".-.."
        | 'm' -> "--"
        | 'n' -> "-."
        | 'o' -> "---"
        | 'p' -> ".--."
        | 'q' -> "--.-"
        | 'r' -> ".-."
        | 's' -> "..."
        | 't' -> "-"
        | 'u' -> "..-"
        | 'v' -> "...-"
        | 'w' -> ".--"
        | 'x' -> "-..-"
        | 'y' -> "-.--"
        | 'z' -> "--.."
        | '1' -> ".----"
        | '2' -> "..---"
        | '3' -> "...--"
        | '4' -> "....-"
        | '5' -> "....."
        | '6' -> "-...."
        | '7' -> "--..."
        | '8' -> "---.."
        | '9' -> "----."
        | '0' -> "-----"
        | _ -> "E"

let recSmorse (string: string) =
    let rec encoder word code =
        match String.length word with
        | 1 -> code + charEncoder word.[0]
        | _ -> encoder (word.Substring 1) (code + (charEncoder word.[0]))
    encoder (string.ToLower ()) ""

let readLines filePath = System.IO.File.ReadLines(filePath);;

[<EntryPoint>]
let main argv =
    let words = readLines "/Users/alexandermolholm/Documents/IKT/6. Semester/AFP/ExamExer/SmoosedMorseCode/enable.txt"
    let keyAndValue = Seq.countBy recSmorse words
    let mutable magicCode = ""
    for i in keyAndValue do
        if ((snd i) = 13) then do
            magicCode <- fst i
    
    printfn "MAGIC CODE: %A" magicCode
    
    printfn "WORDS THAT  CAN BE ENCODED TO MAGIC CODE: "
    for word in  (Seq.filter (fun w -> smorse w = magicCode) words) do
        printfn "%A" word
    
    0