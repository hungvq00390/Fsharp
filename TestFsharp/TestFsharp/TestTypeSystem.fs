module TestTypeSystem
open CoreTypeSystem
open NUnit.Framework
open FsUnit
open AST

// Test Common function

let rec getNextTag (lst1: TagSeq) : TagSeq =
    match lst1 with
    | [] -> []
    | x::xs -> [fst x,1]

// Check is this tag or not
let isTag tag = if (tag = '+' || tag = '-' || tag = '#' || tag = ':' || tag = '(' || tag = ')') then 
                    true 
                else 
                    false

// Remove value of current tag
let rec removeValue (chrlst:char list) =
    match chrlst with
    | [] -> []
    | x::xs -> 
        if (isTag x) then 
            x::xs 
        else removeValue xs

let isEndBranch x = 
    if (x = ')') then 
        true 
    else 
        false
let isStartBranch x = 
    if (x = '(') then 
        true 
    else 
        false

let rec removeBranch (chrlst:char list) (total:int)=
    match chrlst with
    | [] -> []
    | x::xs -> 
        if (isEndBranch x) && (total = 0) then xs 
        elif (isEndBranch x) && (total > 0) then removeBranch xs (total - 1)
        elif (isStartBranch x) then removeBranch xs (total + 1)
        else removeBranch xs 0

// Get value of current tag
let rec getValue (chrlst:char list) =
    match chrlst with
    | [] -> []
    | x::xs -> 
        if (isTag x) 
            then [] 
        else x::getValue xs

// String to char list
let string2charlist (str:string) = [for c in str -> c]
let rec charlist2stringlist (s:char list) = 
    match s with 
    | [] -> []
    | x::xs -> x.ToString()::charlist2stringlist xs
let rec stringToInt str = System.Int32.Parse(str |> String.concat "")
let rec charListToInt str = System.Int32.Parse(charlist2stringlist(str) |> String.concat "")
let rec charList2String s = charlist2stringlist(s) |> String.concat ""

// Change char list to TagSeq list
let rec charList2type (chrlst:char list) : TagSeq=
 match chrlst with
 | [] -> []
 | '+'::xs -> (Tag.Plus, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | '-'::xs -> (Tag.Minus, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | '#'::xs -> (Tag.Max, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | ':'::xs -> (Tag.Join, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | x::xs -> charList2type xs

// Change TagSeq list to TFJ string
let rec type2string (lst:TagSeq) : string =
 match lst with
 | [] -> ""
 | (Tag.Plus, n1)::xs -> "+" + n1.ToString() + type2string xs
 | (Tag.Minus, n1)::xs -> "-" + n1.ToString() + type2string xs
 | (Tag.Max, n1)::xs -> "#" + n1.ToString() + type2string xs
 | (Tag.Join, n1)::xs -> ":" + n1.ToString() + type2string xs
 | x::xs -> x.ToString() + type2string xs

let rec string2Type str : TagSeq = charList2type (string2charlist str);;

// Test Manual
//let s13 = "#2:1:1:1";;
//printfn "Test Join %A" (type2string (join (string2Type s13)));
//
//let s11 = "#2:1:1:1";;
//let s12 = "#0:1#3:1#4:1";;
//let testMerge = merge (string2Type s11) (string2Type s12)
//printfn "Test Merge %s" (type2string (testMerge));;
//
//let s10 = "#1#2-3+2+22+13#12-9#2+3#12-4";;
//let s14 = "#4#5+3#3-4";;
//let s17 = "-1+1+1+1-1-1-1-1+1+1+1+1-1-1-1-1-1";;
//let testCanonical = seq (string2Type s14)
//printfn "Test Canonical %s" (type2string (testCanonical));;
//
//let s15 = "+1";;
//let s16 = "#2:2#3:2#4:2";;
//let testJoinCommit = jc (string2Type s15) (string2Type s16)
//printfn "Test JoinCommit %s" (type2string (testJoinCommit));;
// End Test manual

// Test paper:
//let step1 = join(seq (string2Type ("+1+1-1-1-1-1-1")));;  //#2:1:1:1
//let step2 = merge (step1) (join(seq(string2Type ("-1+1+1+1-1-1-1-1+1+1+1+1-1-1-1-1-1")))) //-1#3-1#4-1  -> #2:2#3:2#4:2
//let step3 = seq (jc (string2Type ("+1")) step2) //#4:2#4:2
//let step4 = merge (join(seq(string2Type ("+1-1-1-1")))) (step3)  //#5:3#4:3 
//let step5 = jc (string2Type ("+2")) step4;;
//printfn "\nTest Example 1: %s" (type2string (step5));;

//let step1a = join(seq (string2Type ("-1")));;
//let step2a = merge step1a (join(seq(string2Type("-1"))));;
//let step3a = seq (jc (string2Type ("+1#1")) step2a);;
//printfn "Test Example 2: %s\n" (type2string (step3a));;

// End Test Paper

// Unit Test
 [<TestCaseSource("TestCases")>]
// Test Join function
let strTest1 = "#2-3";;
let checkJoin = type2string (join (string2Type strTest1)) |> should equal "#2:1:1:1"

// Test Merge function
let strTest2 = "#2:1:1:1";;
let strTest3 = "#0:1#3:1#4:1";;
let checkMerge = type2string (merge (string2Type strTest2) (string2Type strTest3)) |> should equal "#2:2#3:2#4:2"

// Test seq function
let strTest4 = "-1+1+1+1-1-1-1-1+1+1+1+1-1-1-1-1-1";;
let checkCanonical = type2string (seq (string2Type strTest4)) |> should equal "-1#3-1#4-1"
let strTest4a = "#4#5+3#3-4";;
let checkCanonical1 = type2string (seq (string2Type strTest4a)) |> should equal "#6-1"
let strTest4b = "#1#2-3+2+22+13#12-9#2+3#12-4";;
let checkCanonical2 = type2string (seq (string2Type strTest4b)) |> should equal "#2-3+27#22"

// Test join commit function
let strTest5 = "+1";;
let strTest6 = "#2:2#3:2#4:2";;
let checkJoinCommit = type2string (jc (string2Type strTest5) (string2Type strTest6)) |> should equal "#4#3:2#4:2"

//+1-1 -> #1
let ast1 = [Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1)]
let checkAST1 = type2string (infer ast1 []) |> should equal "#1"

//+1+1-1-1 -> #2
let ast2 = [Leaf (Tag.Plus, 1); Leaf (Tag.Plus, 1);Leaf (Tag.Minus, 1);Leaf (Tag.Minus, 1)]
let checkAST2 = type2string (infer ast2 []) |> should equal "#2"

//+1(-1)-1 -> #2
let ast3 = [Leaf (Tag.Plus, 1); Branch ([Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)] 
let checkAST3 = type2string (infer ast3 []) |> should equal "#2"

//+1+1-1(-1)-1 -> #2
let ast4 = [Leaf (Tag.Plus, 1); Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1);Branch ([Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)] 
let checkAST4 = type2string (infer ast4 []) |> should equal "#2"

//+1(+1-1-1)-1 -> #3
let ast5 = [Leaf (Tag.Plus, 1); Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)] 
let checkAST5 = type2string (infer ast5 []) |> should equal "#3"

//+2(+2-2-2)-2   -> #6
let ast6 = [Leaf (Tag.Plus, 2); Branch ([Leaf (Tag.Plus, 2); Leaf (Tag.Minus, 2); Leaf (Tag.Minus, 2)]); Leaf (Tag.Minus, 2)] 
let checkAST6 = type2string (infer ast6 []) |> should equal "#6"

//+2(+1-1-1-1)-1-1 -> #5
let ast7 = [Leaf (Tag.Plus, 2); Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1);Leaf (Tag.Minus, 1)] 
let checkAST7 = type2string (infer ast7 []) |> should equal "#5"

//+2(+1-1-1-1)+1(+2-2-1-1-1)-1+3-3-1+4-4-1
let ast8 = [Leaf (Tag.Plus, 2); Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Plus, 1); Branch ([Leaf (Tag.Plus, 2); Leaf (Tag.Minus, 2); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1); Leaf (Tag.Plus, 3); Leaf (Tag.Minus, 3);Leaf (Tag.Minus, 1);Leaf (Tag.Plus, 4); Leaf (Tag.Minus, 4);Leaf (Tag.Minus, 1)] 
let checkAST8 = type2string (infer ast8 []) |> should equal "#11"

let ast9 = [Leaf (Tag.Plus,1); Leaf (Tag.Plus,1); Leaf (Tag.Minus,1); Branch ([Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)]
let checkAST9 = type2string (infer ast9 []) |> should equal "#2"

//printfn "infer 1: %A = %A" ast8 (infer ast8 []);;
//printfn "infer 2: %A = %A" ast9 (infer ast9 []);;

let rec addTree (lst1: char list) : Tree list = 
    match lst1 with
    | [] -> []
    | '-'::xs -> List.append [Leaf (Tag.Minus, charListToInt(getValue xs))] (addTree (removeValue xs))
    | '+'::xs -> List.append [Leaf (Tag.Plus, charListToInt(getValue xs))] (addTree (removeValue xs))
    | '('::xs -> List.append  [Branch (addTree xs)] (addTree (removeBranch xs 0))
    | ')'::xs -> []
    | x::xs -> addTree xs
        
let calculateTFJstring x = infer (addTree(string2charlist x)) []

printfn "Result: %A" (calculateTFJstring "+2(+1-1-1-1)+1(+2-2-1-1-1)-1+3-3-1+4-4-1");;


// Pause terminal
let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()
pause ()  