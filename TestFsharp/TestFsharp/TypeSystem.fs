// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module TypeSystem

let s1 = ['#';'1';'2';'2';'+';'1';'-';'1';'1';'2']
let s2 = ['#';'1';'+';'1';'-';'1']
let s3 = ['#';'1';'-';'2']
let s4 = []
let s5 = ['#';'1';'#';'2';'-';'3';'#';'2']

/////// Common Function /////////
//Check a in list l
let rec mem a l = 
  match l with
  | [] -> false
  | hd::tl -> hd=a || mem a tl

//Append two list
let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | hd :: tl -> hd :: append tl l2

//Reverse two list
let rec reverse = function 
  | [] -> []
  | hd::tl -> append (reverse tl) [hd]

//Get max value in list
let rec getMaxList l = 
  match l with
  | [] -> failwith "Can't take the minimum of an empty list"
  | [x] -> x
  | x::xs ->
    let maxRest = getMaxList xs
    max x maxRest

let string2char (s:string) = [for c in s -> c]
let rec charlist2stringlist (s:char list) = 
 match s with 
 | [] -> []
 | x::xs -> x.ToString()::charlist2stringlist xs
let rec stringToInt s = System.Int32.Parse(s |> String.concat "")
let rec charToInt s = System.Int32.Parse(charlist2stringlist(s) |> String.concat "")
let rec stringList2String s = s |> String.concat ""
let rec minus m n = m - n
let rec max (a:int) (b:int) = if a > b then a else b

///////End Common Function//////////

let isSignal s = if (s = '+' || s = '-' || s = '#' || s = ':') then true else false
let explode (s:string) = [for c in s -> c]
let string2 = explode "+1-1+1-1+1-1+1+1-1"

let rec sign s = 
  match s with
  | [] -> [""]
  | x::xs -> if isSignal x then 
               x.ToString()::(sign xs)
              else (sign xs)

let rec removeValue (s1:char list) =
 match s1 with
 | [] -> []
 | x::xs -> if (isSignal x) then x::xs else removeValue xs

let rec getValue (s1:char list) =
 match s1 with
 | [] -> []
 | x::xs -> if (isSignal x) then [] else x::getValue xs

//Join function
let rec change (h:int) = if (h> 0) then ":1"::change (h-1) else [""]

let rec joinTemp1 (s1:char list) (s2:char list) =
 match s1 with
 | [] -> change (charToInt s2)
 | x::xs -> if (isSignal x) then change (charToInt s2) else joinTemp1 xs (x::s2) 

let rec joinTemp (s1:char list) =
 match s1 with
 | [] -> [""]
 | x::xs -> if (isSignal x) then if x = '-' then append (joinTemp1 xs []) (joinTemp (removeValue xs))  else x.ToString()::joinTemp xs 
             else x.ToString()::joinTemp xs

let rec join (s:string) = joinTemp (string2char s) |> String.concat ""
//End Join function

//Start Merge function
let rec add (m:char list) (n:char list) = (charToInt(getValue(m)) + charToInt(getValue(n))).ToString()

let rec mergeTemp1 s1 s2 = 
  match s1 with 
  | [] -> [""]
  | x::xs -> match s2 with 
                | [] ->  [""]
                | x2 :: xs2 -> if isSignal x && isSignal x2 then x.ToString() :: (mergeTemp1 xs xs2)
                               else (add (x::xs) (x2::xs2))::(mergeTemp1 (removeValue(x::xs)) (removeValue(x2::xs2)))

let rec mergeTemp s1 s2 = if (sign s1) = (sign s2) then mergeTemp1 s1 s2
                            else ["Fail, can not merge 2 string"]

let rec merge (s:string) (s1:string) = mergeTemp (string2char s) (string2char s1) |> String.concat ""
//End Merge function

let rec getNextSignal s = 
 match s with
 | [] -> ""
 | x::xs -> if isSignal x then x.ToString() else getNextSignal xs

let rec getNextValue s = 
 match s with
 | [] -> []
 | x::xs -> if isSignal x then getValue xs else getNextValue xs

let rec removeSignalValue s h = 
 match s with
 | [] -> []
 | x::xs -> if (isSignal x) && h > 0 then removeValue xs else removeSignalValue xs (h-1)

let s7 = ['#';'1';'#';'2';'-';'3';'+';'2';'+';'2';'2']
//Start Canonical function
let rec CanonicalTemp (s:char list) = 
 match s with
 | [] -> []
 | '#'::xs -> if (getNextSignal xs) = "#" then "#"+ (max (charToInt (getValue xs)) (charToInt(getNextValue xs))).ToString()::(CanonicalTemp(removeSignalValue xs 2)) else "#"::CanonicalTemp xs
 | '+'::xs -> if (getNextSignal xs) = "+" then "+"+ (add (getValue xs) (getNextValue xs)).ToString()::(CanonicalTemp(removeSignalValue xs 2)) else "+"::CanonicalTemp xs
 | '-'::xs -> if (getNextSignal xs) = "-" then "-"+ (add (getValue xs) (getNextValue xs)).ToString()::(CanonicalTemp(removeSignalValue xs 2)) else "-"::CanonicalTemp xs
 | x::xs -> x.ToString()::CanonicalTemp xs

let rec Canonical (s:string) = CanonicalTemp (string2char s) |> String.concat ""

//End Canonical funtion

////// Main Function //////

let s10 = "#1#2-3+2+22";;
let testCanonical = Canonical s10
printfn "\nTest Canonical %s" (testCanonical);;

let s11 = "#1#2-3+2+22";;
let s12 = "#1#2-3+2+22";;
let testMerge = merge s11 s12
printfn "\nTest Merge %s" (testMerge);;

let s13 = "#1#2-3+2+22-3-2";;
let testJoin = join s13
printfn "\nTest join %s" (testJoin);;

///// End Main Function /////



let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()

pause ()  