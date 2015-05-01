// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module TypeSystem
open Microsoft.FSharp.Collections
let s1 = ['#';'1';'2';'2';'+';'1';'-';'1';'1';'2']
let s2 = ['#';'1';'+';'1';'-';'1']
let s3 = ['#';'1';'-';'2']
let s4 = []
let s5 = ['#';'1';'#';'2';'-';'3';'#';'2']

/////// Common Function /////////
//Check a in list l
let rec mem a lst = 
  match lst with
  | [] -> false
  | hd::tl -> hd=a || mem a tl

//Append two list
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | hd :: tl -> hd :: append tl lst2

//Reverse list
let rec reverse = function 
  | [] -> []
  | hd::tl -> append (reverse tl) [hd]

//Get max value in list
let rec getMaxList lst = 
  match lst with
  | [] -> failwith "Can't take the minimum of an empty list"
  | [x] -> x
  | x::xs ->
    let maxRest = getMaxList xs
    max x maxRest

let string2charlist (str:string) = [for c in str -> c]
let rec charlist2stringlist (s:char list) = 
 match s with 
 | [] -> []
 | x::xs -> x.ToString()::charlist2stringlist xs
let rec stringToInt str = System.Int32.Parse(str |> String.concat "")
let rec charListToInt str = System.Int32.Parse(charlist2stringlist(str) |> String.concat "")
let rec charList2String s = charlist2stringlist(s) |> String.concat ""
let rec stringList2String strlst = strlst |> String.concat ""
let rec stringList2charList strlst = string2charlist (strlst |> String.concat "")
let rec int2CharList a = string2charlist(a.ToString())
let rec max (a:int) (b:int) = if a > b then a else b
///////End Common Function//////////

let isTag tag = if (tag = '+' || tag = '-' || tag = '#' || tag = ':') then true else false

let rec sign chrlst = 
  match chrlst with
  | [] -> [""]
  | x::xs -> if isTag x then 
               x.ToString()::(sign xs)
              else (sign xs)

let rec removeValue (chrlst:char list) =
 match chrlst with
 | [] -> []
 | x::xs -> if (isTag x) then x::xs else removeValue xs

let rec getValue (chrlst:char list) =
 match chrlst with
 | [] -> []
 | x::xs -> if (isTag x) then [] else x::getValue xs

//Start Join function
let rec change (index:int) = if (index> 0) then ":1"::change (index-1) else [""]

let rec joinTemp1 (s1:char list) (s2:char list) =
 match s1 with
 | [] -> change (charListToInt s2)
 | x::xs -> if (isTag x) then change (charListToInt s2) else joinTemp1 xs (x::s2) 

let rec joinTemp (s1:char list) =
 match s1 with
 | [] -> [""]
 | x::xs -> if (isTag x) then if x = '-' then append (joinTemp1 xs []) (joinTemp (removeValue xs))  else x.ToString()::joinTemp xs 
             else x.ToString()::joinTemp xs

let rec Join (str:string) = joinTemp (string2charlist str) |> String.concat ""
//End Join function

//Start Merge function
let rec add (chrlst1:char list) (chrlst2:char list) = (charListToInt(getValue(chrlst1)) + charListToInt(getValue(chrlst2))).ToString()

let rec mergeTemp1 chrlst1 chrlst2 = 
  match chrlst1 with 
  | [] -> [""]
  | x::xs -> match chrlst2 with 
                | [] ->  [""]
                | x2 :: xs2 -> if isTag x && isTag x2 then x.ToString() :: (mergeTemp1 xs xs2)
                               else (add (x::xs) (x2::xs2))::(mergeTemp1 (removeValue(x::xs)) (removeValue(x2::xs2)))

let rec mergeTemp s1 s2 = if (sign s1) = (sign s2) then mergeTemp1 s1 s2
                            else ["Fail, can not merge 2 string"]

let rec Merge (s:string) (s1:string) = mergeTemp (string2charlist s) (string2charlist s1) |> String.concat ""
//End Merge function

let rec getNextSignal chrlst index = 
 match chrlst with
 | [] -> ""
 | x::xs -> if (isTag x) then 
               if index = 1 then x.ToString() 
                 else getNextSignal xs (index-1) 
              else getNextSignal xs index

let rec getNextValue chrlst index = 
 match chrlst with
 | [] -> []
 | x::xs -> if (isTag x) then 
               if index = 1 then getValue xs 
                 else getNextValue xs (index-1) 
              else getNextValue xs index

let s6 = ['#';'3';'#';'8';'8';'-';'4';'3';'#';'3';'-';'2'];;
let rec removeSignalValue chrlst index = 
 match chrlst with
 | [] -> []
 | x::xs -> if (isTag x) && index > 0 then removeSignalValue xs (index-1) else if index <= 0 then removeValue chrlst else removeSignalValue xs index

//printf "\nTest removeValue %A" (removeSignalValue s6 3);;

let s7 = ['#';'3';'#';'8';'8';'-';'4';'3';'#';'3';'-';'2'];;
let rec getSignalValue chrlst index = 
 match chrlst with
 | [] -> []
 | x::xs -> if (isTag x) && index > 0 then x::getSignalValue xs (index-1) 
                   else if index > 0 then x::getSignalValue xs index else if index = 0 then getValue chrlst else []

//printf "\nTest getValue %A" (getSignalValue s7 3);;


//Start Canonical function

let rec addValue (chrlst:char list) = System.Int32.Parse(charList2String chrlst) + 1

let rec CanonicalTemp1 (chrlst:char list) = 
 match chrlst with 
 | [] -> ""
 | '#' :: xs -> charlist2stringlist chrlst |> String.concat ""
 | '+' :: xs -> if charListToInt(getValue xs) = 1 then 
                    if getNextSignal xs 2 = "-" then 
                       if charListToInt(getNextValue xs 2) = 1 then charList2String ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1))
                         else charList2String (append ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1)) ('-' :: int2CharList(charListToInt(getNextValue xs 2) - 1)))
                      else charList2String ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1))
                  else if getNextSignal xs 2 = "-" then if charListToInt(getNextValue xs 2) = 1 then CanonicalTemp1 (append ('+' :: int2CharList(charListToInt(getValue xs) - 1)) ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1)))
                                                           else CanonicalTemp1 (append (append ('+' :: int2CharList(charListToInt(getValue xs) - 1)) ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1))) ('-' :: int2CharList(charListToInt(getNextValue xs 2) - 1)))
                          else CanonicalTemp1 (append ('+' :: int2CharList(charListToInt(getValue xs) - 1)) ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1)))
 | x::xs -> charlist2stringlist chrlst |> String.concat ""

let rec CanonicalTemp (chrlst:char list) = 
 match chrlst with
 | [] -> []
 | '#'::xs -> if (getNextSignal xs 1) = "#" then CanonicalTemp (stringList2charList ("#"+ (max (charListToInt (getValue xs)) (charListToInt(getNextValue xs 1))).ToString()::(CanonicalTemp(removeSignalValue xs 1)))) else "#"::CanonicalTemp xs
 | '+'::xs -> if (getNextSignal xs 1) = "+" then CanonicalTemp (stringList2charList ("+"+ (add (getValue xs) (getNextValue xs 1)).ToString()::(CanonicalTemp(removeSignalValue xs 1))))
                else if (getNextSignal xs 1 = "#") && (getNextSignal xs 2 = "-") then CanonicalTemp (append (string2charlist(CanonicalTemp1 (getSignalValue chrlst 3))) (removeSignalValue xs 3)) 
                       else "+"::CanonicalTemp xs
 | '-'::xs -> if (getNextSignal xs 1) = "-" then CanonicalTemp (stringList2charList ("-"+ (add (getValue xs) (getNextValue xs 1)).ToString()::(CanonicalTemp(removeSignalValue xs 2)))) else "-"::CanonicalTemp (xs)
 | x::xs -> x.ToString()::CanonicalTemp xs

let rec Canonical (str:string) = CanonicalTemp (string2charlist str) |> String.concat ""

//End Canonical funtion//

//Start Choice//
let rec choiceTemp (chrlst1:char list) (chrlst2:char list) =
 match chrlst1 with
 | [] -> []
 | '#'::xs -> match chrlst2 with 
              | [] -> []
              | '#'::xs2-> if charList2String (removeValue xs) = charList2String (removeValue xs2) then if charListToInt (getValue xs2) > charListToInt (getValue xs2) then s1 else s2
                             else []
              | x::xs2 -> []
 | x::xs -> []
let rec Choice (s1:string) (s2:string) = if (charList2String (choiceTemp (string2charlist s1) (string2charlist s2))) = "" then "Can not choice" else charList2String (choiceTemp (string2charlist s1) (string2charlist s2))
//End Choice//


////// Test Function //////
let s10 = "#1#2-3+2+22+13#12-9#2+3#12-4";;
let s14 = "#4#5+3#3-4";;
let testCanonical = Canonical s10
printfn "\nTest Canonical %s" (testCanonical);;

let s11 = "#1#2-3+2+22";;
let s12 = "#1#2-3+2+22";;
let testMerge = Merge s11 s12
printfn "\nTest Merge %s" (testMerge);;

let s13 = "-1#2-3+2+22-3-2";;
let testJoin = Join s13
printfn "\nTest join %s" (testJoin);;

let s17 = "#2-3+2+22-3-2";;
let s18 = "#4-3+2+22-3-2";;
let testChoice = Choice s17 s18
printfn "\nTest Choice: %s" (testChoice);;

///// End Main Function /////
let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()

pause ()  