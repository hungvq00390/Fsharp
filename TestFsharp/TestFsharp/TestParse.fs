// Learn more about F# at http://fsharp.net
module TestParse
open System.IO
open Microsoft.FSharp.Text.Lexing

let testLexerAndParserFromFile (fileName:string) expectedString = 
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader

    let parseToTFJ = Parser.start Lexer.tokenstream lexbuf

    printfn "Result = %A, expected %A" parseToTFJ expectedString

let testFile = Path.Combine(__SOURCE_DIRECTORY__, "test.txt")
testLexerAndParserFromFile testFile []

printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


