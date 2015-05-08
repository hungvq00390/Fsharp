// Learn more about F# at http://fsharp.net
module TestParse
open System.IO
open Microsoft.FSharp.Text.Lexing

let mutable tokenList = [""]
let rec testLexerAndParserFromFile (fileName:string) = 
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    let mutable keepParsing = true
    while keepParsing = true do
    let parseToTFJ = Parser.start Lexer.tokenstream lexbuf
    if parseToTFJ <> [""] then tokenList <- tokenList @ (parseToTFJ)
    if lexbuf.IsPastEndOfStream then keepParsing <- false
    

let testFile = Path.Combine(__SOURCE_DIRECTORY__, "test.txt")
let check = testLexerAndParserFromFile testFile;;
printfn "Result = %A" (tokenList |> String.concat "")
printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


