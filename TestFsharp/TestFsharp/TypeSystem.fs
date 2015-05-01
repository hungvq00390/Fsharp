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

//Reverse list
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

let string2charlist (s:string) = [for c in s -> c]
let rec charlist2stringlist (s:char list) = 
 match s with 
 | [] -> []
 | x::xs -> x.ToString()::charlist2stringlist xs
let rec stringToInt s = System.Int32.Parse(s |> String.concat "")
let rec charListToInt s = System.Int32.Parse(charlist2stringlist(s) |> String.concat "")
let rec charList2String s = charlist2stringlist(s) |> String.concat ""
let rec stringList2String s = s |> String.concat ""
let rec stringList2charList s = string2charlist (s |> String.concat "")

let rec int2CharList s = string2charlist(s.ToString())

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
 | [] -> change (charListToInt s2)
 | x::xs -> if (isSignal x) then change (charListToInt s2) else joinTemp1 xs (x::s2) 

let rec joinTemp (s1:char list) =
 match s1 with
 | [] -> [""]
 | x::xs -> if (isSignal x) then if x = '-' then append (joinTemp1 xs []) (joinTemp (removeValue xs))  else x.ToString()::joinTemp xs 
             else x.ToString()::joinTemp xs

let rec Join (s:string) = joinTemp (string2charlist s) |> String.concat ""
//End Join function

//Start Merge function
let rec add (m:char list) (n:char list) = (charListToInt(getValue(m)) + charListToInt(getValue(n))).ToString()

let rec mergeTemp1 s1 s2 = 
  match s1 with 
  | [] -> [""]
  | x::xs -> match s2 with 
                | [] ->  [""]
                | x2 :: xs2 -> if isSignal x && isSignal x2 then x.ToString() :: (mergeTemp1 xs xs2)
                               else (add (x::xs) (x2::xs2))::(mergeTemp1 (removeValue(x::xs)) (removeValue(x2::xs2)))

let rec mergeTemp s1 s2 = if (sign s1) = (sign s2) then mergeTemp1 s1 s2
                            else ["Fail, can not merge 2 string"]

let rec Merge (s:string) (s1:string) = mergeTemp (string2charlist s) (string2charlist s1) |> String.concat ""
//End Merge function

let rec getNextSignal s h = 
 match s with
 | [] -> ""
 | x::xs -> if (isSignal x) then 
               if h = 1 then x.ToString() 
                 else getNextSignal xs (h-1) 
              else getNextSignal xs h

let rec getNextValue s h = 
 match s with
 | [] -> []
 | x::xs -> if (isSignal x) then 
               if h = 1 then getValue xs 
                 else getNextValue xs (h-1) 
              else getNextValue xs h

let s6 = ['#';'3';'#';'8';'8';'-';'4';'3';'#';'3';'-';'2'];;
let rec removeSignalValue s h = 
 match s with
 | [] -> []
 | x::xs -> if (isSignal x) && h > 0 then removeSignalValue xs (h-1) else if h <= 0 then removeValue s else removeSignalValue xs h

//printf "\nTest removeValue %A" (removeSignalValue s6 3);;

let s7 = ['#';'3';'#';'8';'8';'-';'4';'3';'#';'3';'-';'2'];;
let rec getSignalValue s h = 
 match s with
 | [] -> []
 | x::xs -> if (isSignal x) && h > 0 then x::getSignalValue xs (h-1) 
                   else if h > 0 then x::getSignalValue xs h else if h = 0 then getValue s else []

//printf "\nTest getValue %A" (getSignalValue s7 3);;


//Start Canonical function

let rec addValue (s:char list) = System.Int32.Parse(charList2String s) + 1

let rec CanonicalTemp1 (s:char list) = 
 match s with 
 | [] -> ""
 | '#' :: xs -> charlist2stringlist s |> String.concat ""
 | '+' :: xs -> if charListToInt(getValue xs) = 1 then 
                    if getNextSignal xs 2 = "-" then 
                       if charListToInt(getNextValue xs 2) = 1 then charList2String ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1))
                         else charList2String (append ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1)) ('-' :: int2CharList(charListToInt(getNextValue xs 2) - 1)))
                      else charList2String ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1))
                  else if getNextSignal xs 2 = "-" then if charListToInt(getNextValue xs 2) = 1 then CanonicalTemp1 (append ('+' :: int2CharList(charListToInt(getValue xs) - 1)) ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1)))
                                                           else CanonicalTemp1 (append (append ('+' :: int2CharList(charListToInt(getValue xs) - 1)) ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1))) ('-' :: int2CharList(charListToInt(getNextValue xs 2) - 1)))
                          else CanonicalTemp1 (append ('+' :: int2CharList(charListToInt(getValue xs) - 1)) ('#' :: int2CharList(charListToInt(getNextValue xs 1) + 1)))
 | x::xs -> charlist2stringlist s |> String.concat ""

let rec CanonicalTemp (s:char list) = 
 match s with
 | [] -> []
 | '#'::xs -> if (getNextSignal xs 1) = "#" then CanonicalTemp (stringList2charList ("#"+ (max (charListToInt (getValue xs)) (charListToInt(getNextValue xs 1))).ToString()::(CanonicalTemp(removeSignalValue xs 1)))) else "#"::CanonicalTemp xs
 | '+'::xs -> if (getNextSignal xs 1) = "+" then CanonicalTemp (stringList2charList ("+"+ (add (getValue xs) (getNextValue xs 1)).ToString()::(CanonicalTemp(removeSignalValue xs 1))))
                else if (getNextSignal xs 1 = "#") && (getNextSignal xs 2 = "-") then CanonicalTemp (append (string2charlist(CanonicalTemp1 (getSignalValue s 3))) (removeSignalValue xs 3)) 
                       else "+"::CanonicalTemp xs
 | '-'::xs -> if (getNextSignal xs 1) = "-" then CanonicalTemp (stringList2charList ("-"+ (add (getValue xs) (getNextValue xs 1)).ToString()::(CanonicalTemp(removeSignalValue xs 2)))) else "-"::CanonicalTemp (xs)
 | x::xs -> x.ToString()::CanonicalTemp xs

let rec Canonical (s:string) = CanonicalTemp (string2charlist s) |> String.concat ""

//End Canonical funtion

////// Main Function //////

let s10 = "#1#2-3+2+22+13#12-9#2+3#12-4";;
let s20 = "#4#5+3#3-4";;
let testCanonical = Canonical s10
printfn "\nTest Canonical %s" (testCanonical);;

let s11 = "#1#2-3+2+22";;
let s12 = "#1#2-3+2+22";;
let testMerge = Merge s11 s12
printfn "\nTest Merge %s" (testMerge);;

let s13 = "-1#2-3+2+22-3-2";;
let testJoin = Join s13
printfn "\nTest join %s" (testJoin);;

///// End Main Function /////



let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()

pause ()  