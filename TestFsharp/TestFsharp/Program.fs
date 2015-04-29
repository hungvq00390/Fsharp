// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Test
let rec mem a l = 
  match l with
  | [] -> false
  | hd::tl -> hd=a || mem a tl

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | hd :: tl -> hd :: append tl l2

let rec reverse = function 
  | [] -> []
  | hd::tl -> append (reverse tl) [hd]

let rec minus m n = m - n;;
                 
let rec max a b = if a > b then a else b

let rec getMaxList l = 
  match l with
  | [] -> failwith "Can't take the minimum of an empty list"
  | [x] -> x
  | x::xs ->
    let maxRest = getMaxList xs
    max x maxRest
       
printfn "%A" (getMaxList [3;5;6;7;8]);;
printfn "%A" (max 7 1);;

let code = []
let rec parse test = 
  match test with 
  | [] -> []
  | x::xs -> if x = "onacid" then 
                "+1"::(parse xs)
              else if x = "spawn" then 
               "(1"::(parse xs)
              else if x = "commit" then 
               "-1"::(parse xs)
              else if x = "endspawn" then 
               ")"::(parse xs)
              else (parse xs)

let isSignal s = if (s = '+' || s = '-' || s = '#' || s = ':') then true else false

let explode (s:string) = [for c in s -> c]
let string2 = explode "+1-1+1-1+1-1+1+1-1"

let rec balanceStr string2 = 
  match string2 with 
  | [] -> 0
  | x::xs -> if x = '+' then 
                1 + (balanceStr xs)
              else if x = '-' then 
               -1 + (balanceStr xs)
               else (balanceStr xs)
printfn "Test BalanceString %A" (balanceStr string2);;

let string2char (s:string) = [for c in s -> c]
let rec charlist2stringlist (s:char list) = 
 match s with 
 | [] -> []
 | x::xs -> x.ToString()::charlist2stringlist xs

let rec sign s = 
  match s with
  | [] -> [""]
  | x::xs -> if isSignal x then 
               x.ToString()::(sign xs)
              else (sign xs)

let rec add m n = (System.Int32.Parse(m.ToString()) + System.Int32.Parse(n.ToString())).ToString()
let rec summary s1 s2 = 
  match s1 with 
  | [] -> [""]
  | x::xs -> match s2 with 
                | [] ->  [""]
                | x2 :: xs2 -> if isSignal x && isSignal x2 then x.ToString() :: (summary xs xs2)
                               else (add x x2)::(summary xs xs2)

let rec sum s1 s2 = if (sign s1) = (sign s2) then summary s1 s2
                     else ["Can not calculate sum of 2 string"]

let isSharpNtemp s = 
  match s with
  | [] -> false
  | x::[] -> false
  | x::y::xs -> if (x = '#') && (y <> '#') then true
                  else false

let isSharpN s = if s = "" then false
                  else isSharpNtemp (string2char s)

let s1 = ['#';'1';'+';'1';'-';'1']
let s2 = ['#';'1';'+';'1';'-';'1']
let s3 = ['#';'1';'-';'2']

let rec calWeightTemp (s:char list) (h1:int) = 
 match s with
 | [] -> [""]
 | x::tl -> if (h1 > 0) then calWeightTemp (tl) (h1 - 1) 
             else if (isSignal x) = false then x.ToString()::(calWeightTemp (tl) (h1 - 1))
               else [""]
let calWeight s h = (calWeightTemp s h) |> String.concat ""

printfn "Test calWeigh %s" (calWeight s1 1);;

let test = calWeight s1 2

let rec calSmallStemp (s:char list) (h:int) =
 match s with
 | [] -> [""]
 | x::tl -> if (h > 0) then calSmallStemp (tl) (h - 1) else x.ToString() :: if (isSignal x) = false then (calSmallStemp tl (h-1)) else [""]

let calSmallS (s:char list) (h:int) (ch:int) = 
    if (ch > 1) then reverse (calSmallStemp (reverse s) (s.Length - h)) |> String.concat "" else "#0"

printfn "Test calSmallS %s" (calSmallS s3 2 2);;

let rec getAfterClusterIndexTemp (s:char list) (h:int) (count:int) =
 match s with
 | [] -> -1
 | x::xs -> if (h >= 0) then getAfterClusterIndexTemp xs (h-1) count else if isSignal x then count else getAfterClusterIndexTemp xs (h-1) (count + 1)

let getAfterClusterIndex (s:char list) (h:int) = 
    if (getAfterClusterIndexTemp s h 0) > -1 then (getAfterClusterIndexTemp s h 0) else s.Length
    
let rec calBigsTemp (s:char list) (h:int) = 
 match s with 
 | [] -> [""]
 | x::xs -> if s.Length = getAfterClusterIndex s h then [""] else if (h> 0) then (x.ToString()::calBigsTemp xs (h-1)) else [""]

let calBigs (s:char list) (h:int) = calBigsTemp s h |> String.concat ""
printfn "Test calBig %s" (calBigs s3 2);;

// TO-DO
let s4 = []
let s5 = ['#';'1';'-';'2']
let rec join (s1:char list) (s2:char list) = 
  match s1 with
  | [] -> match s2 with 
          | [] -> [""]
          | x::xs -> charlist2stringlist (x::xs)
  | x::xs -> match s2 with 
          | [] -> charlist2stringlist (x::xs)
          | hd::tl -> if (isSharpN (charlist2stringlist(x::xs) |> String.concat "")) && (isSharpN(charlist2stringlist(hd::tl) |> String.concat "")) 
                        then sum s1 s2 else charlist2stringlist (hd::tl)
printfn "Test Join %A" (join s4 s5);;

let list = sum s1 s2
list |> List.iter (fun x -> printf "%s" x)

let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()

pause ()  