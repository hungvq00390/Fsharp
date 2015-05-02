module CoreTypeSystem
open Microsoft.FSharp.Collections
type Tag =
   | Plus = 0
   | Minus = 1
   | Max = 2
   | Join = 3

type TagNum = Tag * int

type TagSeq = TagNum list

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

// Start Canonical function
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
                                                       else if (xs.Length > 1) then if fst (List.head xs) = Tag.Max && fst (List.head(List.tail xs)) = Tag.Minus then Canonical (List.append (CanonicalTemp (Tag.Plus, n1) (List.head xs) (List.head(List.tail xs))) (List.tail xs))
                                                                                        else (Tag.Plus, n1)::Canonical xs
                                                            else xs
                               else (Tag.Plus, n1)::[]
    | (Tag.Minus, n1)::xs -> if (xs.Length > 0) then if fst (List.head xs) = Tag.Minus then Canonical ((Tag.Minus, n1 + (snd (List.head xs)))::(List.tail xs)) 
                                                        else (Tag.Minus, n1)::Canonical xs
                               else (Tag.Minus, n1)::[]
    | x::xs -> [x]
// Define join function
let rec join (lst: TagSeq) : TagSeq =
    match lst with
    | [] -> []
    | (Tag.Minus, n1)::xs -> if n1 > 0 then (Tag.Join, 1) :: (join ((Tag.Minus, n1-1)::xs)) else (join xs)
    | x::xs -> x::(join xs)

// Define Merge function
let rec merge (lst1: TagSeq) (lst2: TagSeq) : TagSeq = 
    if lst1.Length <= 0 || lst2.Length <= 0 then [] else
    let tag1 = fst (List.head lst1) in
    let tag2 = fst (List.head lst2) in
    if tag1 = Tag.Max && tag2 = Tag.Max then (Tag.Max, (snd (List.head lst1)) + (snd (List.head lst2))) :: (merge (List.tail lst1) (List.tail lst2))
    else if tag1 = Tag.Join && tag2 = Tag.Join then (Tag.Join, (snd (List.head lst1)) + (snd (List.head lst2))) :: (merge (List.tail lst1) (List.tail lst2)) 
    else if tag1 = Tag.Max && tag2 = Tag.Join then (Tag.Join, snd (List.head lst1)) :: (merge (List.tail lst1) lst2) 
    else if tag1 = Tag.Join && tag2 = Tag.Max then (Tag.Join, snd (List.head lst2)) :: (merge lst1 (List.tail lst2)) 
    else failwith "Error in merge"

// Define Join commit function
let rec jc (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    match lst1 with
    | [] -> []
    | (Tag.Plus,m1)::[] -> match lst2 with
                           | [] -> []
                           | (Tag.Max,n1)::(Tag.Join,n2)::xs2 -> if m1 > 1 then jc [(Tag.Plus,m1-1)] (Canonical((Tag.Max,n1+n2) :: xs2)) else (Tag.Max,n1+n2) :: xs2
                           | x::xs -> []
    | x::xs -> []

// Define Choice function
let rec choice (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    match lst1 with
    | [] -> []
    | x::xs -> match  lst2 with
               | [] -> []
               | x2::xs2 -> if (fst x) = (fst x2) && (xs = xs2) && (fst x) = Tag.Max then if (snd x >= snd x2) then lst1 else lst2
                              else []
