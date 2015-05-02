module TestTypeSystem
open CoreTypeSystem
///// Test Mainfunction /////

let rec getNextTag (lst1: TagSeq) : TagSeq =
    match lst1 with
    | [] -> []
    | x::xs -> [fst x,1]

let isTag tag = if (tag = '+' || tag = '-' || tag = '#' || tag = ':') then true else false
let rec removeValue (chrlst:char list) =
 match chrlst with
 | [] -> []
 | x::xs -> if (isTag x) then x::xs else removeValue xs

let rec getValue (chrlst:char list) =
 match chrlst with
 | [] -> []
 | x::xs -> if (isTag x) then [] else x::getValue xs

let string2charlist (str:string) = [for c in str -> c]
let rec charlist2stringlist (s:char list) = 
 match s with 
 | [] -> []
 | x::xs -> x.ToString()::charlist2stringlist xs
let rec stringToInt str = System.Int32.Parse(str |> String.concat "")
let rec charListToInt str = System.Int32.Parse(charlist2stringlist(str) |> String.concat "")
let rec charList2String s = charlist2stringlist(s) |> String.concat ""

let rec charList2type (chrlst:char list) : TagSeq=
 match chrlst with
 | [] -> []
 | '+'::xs -> (Tag.Plus, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | '-'::xs -> (Tag.Minus, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | '#'::xs -> (Tag.Max, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | ':'::xs -> (Tag.Join, charListToInt(getValue xs)):: charList2type (removeValue xs)
 | x::xs -> charList2type xs

let rec type2string (lst:TagSeq) : string =
 match lst with
 | [] -> ""
 | (Tag.Plus, n1)::xs -> "+" + n1.ToString() + type2string xs
 | (Tag.Minus, n1)::xs -> "-" + n1.ToString() + type2string xs
 | (Tag.Max, n1)::xs -> "#" + n1.ToString() + type2string xs
 | (Tag.Join, n1)::xs -> ":" + n1.ToString() + type2string xs
 | x::xs -> x.ToString() + type2string xs

let rec string2Type str : TagSeq = charList2type (string2charlist str)

///// Test /////
let s13 = "-1#2-3+2+22-3-2";;
printfn "Test Join %A" (type2string (join (string2Type s13)));

let s11 = "#1#2:3";;
let s12 = "#1#2:3";;
let testMerge = merge (string2Type s11) (string2Type s12)
printfn "\nTest Merge %s" (type2string (testMerge));;

let s10 = "#1#2-3+2+22+13#12-9#2+3#12-4";;
let s14 = "#4#5+3#3-4";;
let testCanonical = Canonical (string2Type s14)

let s15 = "+2";;
let s16 = "#5:3#4:3";;
let testJoinCommit = jc (string2Type s15) (string2Type s16)
printfn "\nTest JoinCommit %s" (type2string (testJoinCommit));;
///// End Main Function /////

let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()
pause ()  