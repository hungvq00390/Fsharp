// Learn more about F# at http://fsharp.net
module ParseTFJ
open System.IO
open Microsoft.FSharp.Text.Lexing
open TestTypeSystem

let mutable countWork = 0;;
let mutable tokenList = [""]

let rec parseWorkTemp (h:int) (str: string list) : string list = 
    if h > 0 then parseWorkTemp (h-1) (List.append (List.append ["+1"] str) ["-1"])
    else str;;

let parseWork (str: string list) : string list =
    countWork <- countWork + 1
    parseWorkTemp countWork str
    
let rec testLexerAndParserFromFile (fileName:string) = 
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    let mutable keepParsing = true
    while keepParsing = true do
    let parseToTFJ = Parser.start Lexer.tokenstream lexbuf
    if parseToTFJ = ["#"] then tokenList <- tokenList @ (parseWork [])
        elif parseToTFJ <> [""] then tokenList <- tokenList @ (parseToTFJ)
    if lexbuf.IsPastEndOfStream then keepParsing <- false


let getFile (fileName:string) = Path.Combine(__SOURCE_DIRECTORY__, fileName)
let calculateTFJfromFile (fileName:string) = 
    let run = testLexerAndParserFromFile (getFile(fileName))
    let result = calculateTFJstring (tokenList |> String.concat "")
    printfn "Final result: %A" result


let run = calculateTFJfromFile "test1.txt"

printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore