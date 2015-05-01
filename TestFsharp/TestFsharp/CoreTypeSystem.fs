module CoreTypeSystem
type Tag =
   | Plus = 0
   | Minus = 1
   | Max = 2
   | Join = 3

type TagNum = Tag * int

type TagSeq = TagNum list

let rec max (a:int) (b:int) = if a > b then a else b
let rec append (lst1: TagSeq) (lst2: TagSeq): TagSeq =
   match lst1 with 
   | [] -> []
   | hd :: tl -> hd :: append tl lst2

let e55 = [(Tag.Plus,1); (Tag.Plus,1); (Tag.Minus,3);(Tag.Plus,1);(Tag.Minus,1);(Tag.Plus,1);(Tag.Minus,1);]

// Define seq function
let rec seq (lst: TagSeq) : TagSeq =
    match lst with
    | [] -> []
    | (_, 0)::xs -> seq xs
    | (Tag.Plus, n1)::(Tag.Plus, n2) :: xs -> seq ((Tag.Plus,n1+n2)::xs)
    | (Tag.Minus, n1)::(Tag.Minus, n2) :: xs -> seq ((Tag.Minus,n1+n2)::xs)
    | (Tag.Plus, n1)::(Tag.Minus, n2) :: xs -> 
        if n1 >= n2 then 
            seq ((Tag.Plus,n1-n2)::(Tag.Max,n2)::xs) 
        else 
            seq ((Tag.Max,n1)::(Tag.Minus,n2-n1)::xs) 
    | (Tag.Plus, n1)::(Tag.Max, n)::(Tag.Minus, n2) :: xs -> 
        let m = min n1 n2 in seq ((Tag.Plus,n1-m)::(Tag.Max, n+m)::(Tag.Max,n2-m)::xs) 
    | x::xs -> x :: (seq xs)

// Define join function
let rec join (lst: TagSeq) : TagSeq =
    match lst with
    | [] -> []
    | (Tag.Minus, n1)::xs -> if n1 > 0 then (Tag.Join, 1) :: (join ((Tag.Minus, n1-1)::xs)) else (join xs)
    | x::xs -> x::(join xs)

// Define Merge function
let rec merge (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    let tag1 = fst (List.head lst1) in
    let tag2 = fst (List.head lst2) in
    if tag1 = Tag.Max && tag2 = Tag.Max then (Tag.Max, (snd (List.head lst1)) + (snd (List.head lst2))) :: (merge (List.tail lst1) (List.tail lst2))
    else if tag1 = Tag.Join && tag2 = Tag.Join then (Tag.Join, (snd (List.head lst1)) + (snd (List.head lst2))) :: (merge (List.tail lst1) (List.tail lst2)) 
    else if tag1 = Tag.Max && tag2 = Tag.Join then (Tag.Join, snd (List.head lst1)) :: (merge (List.tail lst1) lst2) 
    else if tag1 = Tag.Join && tag2 = Tag.Max then (Tag.Join, snd (List.head lst2)) :: (merge lst1 (List.tail lst2)) 
    else failwith "Error in merge"

let rec getTotalSign (lst: TagSeq) = 
    match lst with
     | [] -> []
     | (Tag.Plus, n1)::xs -> (Tag.Plus, 1) :: getTotalSign xs
     | (Tag.Minus, n1)::xs -> (Tag.Minus, 1) :: getTotalSign xs
     | (Tag.Max, n1)::xs -> (Tag.Max, 1) :: getTotalSign xs
     | (Tag.Join, n1)::xs -> (Tag.Join, 1) :: getTotalSign xs
     | x::xs -> []

let rec mergeNew (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    match lst1 with 
    | [] -> []
    | x::xs -> match lst2 with
                | [] -> []
                | x2::xs2 -> if (getTotalSign lst1) = (getTotalSign lst2) then (fst x, (snd x) + (snd x2))::mergeNew xs xs2 else []

// Define Join commit function
let rec jc (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    let rev1 = List.rev lst1 in
    let tag1 = fst (List.head rev1) in
    let tag2 = fst (List.head lst2) in
    [] //todo

let rec getNextTag (lst1: TagSeq) : TagSeq =
    match lst1 with
    | [] -> []
    | x::xs -> [fst x,1]

let rec CanonicalTemp (lst1: TagNum) (lst2: TagNum) (lst3: TagNum) : TagSeq =
    if  (snd lst1) > (snd lst3) then (Tag.Plus, ((snd lst1) - (snd lst3)))::[Tag.Max, ((snd lst2) + (snd lst1))]
        else if (snd lst1) = (snd lst3) then [Tag.Max, (snd lst2) + (snd lst1)]
                else (Tag.Max, snd lst2 + snd lst1) :: [Tag.Minus, ((snd lst3) - (snd lst1))]

// Define Canonical function
let rec Canonical (lst1: TagSeq) : TagSeq =
    match lst1 with 
    | [] -> []
    | (Tag.Max, n1)::xs -> if (xs.Length > 0) then if fst (List.head xs) = Tag.Max then Canonical ((Tag.Max, max n1 (snd (List.head xs)))::Canonical (List.tail xs)) 
                                                     else (Tag.Max, n1)::Canonical xs 
                              else (Tag.Max, n1)::[]
    | (Tag.Plus, n1)::xs -> if (xs.Length > 0) then if fst (List.head xs) = Tag.Plus then Canonical ((Tag.Plus, n1 + (snd (List.head xs)))::(List.tail xs)) 
                                                       else if (xs.Length > 1) then if fst (List.head xs) = Tag.Max && fst (List.head(List.tail xs)) = Tag.Minus then Canonical (append (CanonicalTemp (Tag.Plus, n1) (List.head xs) (List.head(List.tail xs))) (List.tail xs))
                                                                                        else (Tag.Plus, n1)::Canonical xs
                                                            else xs
                               else (Tag.Plus, n1)::[]
    | (Tag.Minus, n1)::xs -> if (xs.Length > 0) then if fst (List.head xs) = Tag.Minus then Canonical ((Tag.Minus, n1 + (snd (List.head xs)))::(List.tail xs)) 
                                                        else (Tag.Minus, n1)::Canonical xs
                               else (Tag.Minus, n1)::[]
    | x::xs -> []

///// Test Mainfunction /////

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
let testMerge = mergeNew (string2Type s11) (string2Type s12)
printfn "\nTest Merge %s" (type2string (testMerge));;

let s10 = "#1#2-3+2+22+13#12-9#2+3#12-4";;
let s14 = "#4#5+3#3-4";;
let testCanonical = Canonical (string2Type s14)
printfn "\nTest Canonical %s" (type2string (testCanonical));;
///// End Main Function /////

let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()

pause ()  