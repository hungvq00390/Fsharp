module ParseTFJ
open System.IO
open Microsoft.FSharp.Text.Lexing
open TestTypeSystem

let mutable countWork = 0;;
let mutable tokenList = [""]

// Define TFJ string for each work Ex: #1 -> +1-1
let rec parseWorkTemp (h:int) (str: string list) : string list = 
    if h > 0 then 
        parseWorkTemp (h-1) (List.append (List.append ["+1"] str) ["-1"])
    else str;;

// Define level for each work
let parseWork (str: string list) : string list =
    countWork <- countWork + 1
    parseWorkTemp countWork str

// Define parse from txt to TFJ string
let rec processLexerAndParserFromFile (fileName:string) = 
    countWork <- 0
    tokenList <- [""]
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    let mutable keepParsing = true
    while keepParsing = true do
        let parseToTFJ = Parser.start Lexer.tokenstream lexbuf
        if parseToTFJ = ["#"] then 
            tokenList <- tokenList @ (parseWork [])
        elif parseToTFJ <> [""] then 
            tokenList <- tokenList @ (parseToTFJ)
        if lexbuf.IsPastEndOfStream then 
            keepParsing <- false

// Define file location
let getFile (fileName:string) = Path.Combine(__SOURCE_DIRECTORY__, fileName)

// Define calculate from File
let calculateTFJfromFile (fileName:string) = 
    let run = processLexerAndParserFromFile (getFile(fileName))
    let result = calculateTFJstring (tokenList |> String.concat "")
    printfn "Final result: %A" result

// Run test file
let run = calculateTFJfromFile "test.txt"
let run1 = calculateTFJfromFile "test1.txt"
let run2 = calculateTFJfromFile "test2.txt"

printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore