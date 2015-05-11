module CoreTypeSystem
open Heap
open ST
open Microsoft.FSharp.Collections

type Tag =
   | Plus
   | Minus
   | Max
   | Join

type TagNum = Tag * int

type TagSeq = list TagNum

let max a b = if a >= b then a else b
let min a b = if a >= b then b else a
val length: list 'a -> Tot nat

let rec length l = match l with
	| [] -> 0
	| _ :: tl -> 1 + length tl
  
let head l = match l with
	| [] -> failwith "Empty list"
	| hd :: tl -> hd

let tail l = match l with
	| [] -> failwith "Empty list"
	| hd :: tl -> tl
  
val append : l1:list 'a -> l2:list 'a -> Tot (l:list 'a{length l = length l1 + length l2})
let rec append l1 l2 = 
	match l1 with
	| [] -> l2
	| hd :: tl -> hd :: append tl l2
   
// Define seq function
let rec seq (lst: TagSeq) : TagSeq =
    match lst with
    | [] -> failwith "Empty list"
    | (_, 0)::xs -> seq xs
    | (Plus, n1)::(Plus, n2) :: xs -> seq ((Plus,n1+n2)::xs)
    | (Max, n1)::(Max, n2) :: xs -> seq ((Max,max n1 n2)::seq xs)
    | (Minus, n1)::(Minus, n2)::xs -> seq ((Minus,n1+n2)::xs)
    | (Plus, n1)::(Minus, n2)::xs ->
        if n1 >= n2 then
            seq ((Plus,n1-n2)::(Max,n2)::xs)
        else
            seq ((Max,n1)::(Minus,n2-n1)::xs)
    | (Plus, n1)::(Max, n)::(Minus, n2) :: xs ->
        let m = min n1 n2 in
            if n1 > n2 then
                seq ((Plus,n1-m)::(Max, n+m)::xs)
            else if n1 = n2 then
                seq ((Max, n+m)::xs)
            else
                seq ((Max, n+m)::(Minus,n2-m)::xs)
    | x::xs -> x::(seq xs)

// Define join function
let rec join (lst: TagSeq) : TagSeq =
    match lst with
    | [] -> failwith "Empty list"
    | (Minus, n1)::xs -> 
        if n1 > 0 then 
            (Join, 1) :: (join ((Minus, n1-1)::xs)) 
        else 
            (join xs)
    | x::xs -> x::(join xs)

// Define merge function
let rec merge (lst1: TagSeq) (lst2: TagSeq) : TagSeq = 
    if length lst1 = 0 then lst2 
        else if length lst2 = 0 then lst1 
    else
        let tag1 = fst (head lst1) in
        let tag2 = fst (head lst2) in
        if tag1 = Max && tag2 = Max then 
            (Max, (snd (head lst1)) + (snd (head lst2))) :: (merge (tail lst1) (tail lst2))
        else if tag1 = Join && tag2 = Join then 
            (Join, (snd (head lst1)) + (snd (head lst2))) :: (merge (tail lst1) (tail lst2)) 
        else if tag1 = Max && tag2 = Join then 
            (Max, snd (head lst1)) :: (merge (tail lst1) lst2) 
        else if tag1 = Join && tag2 = Max then 
            (Max, snd (head lst2)) :: (merge lst1 (tail lst2)) 
        else failwith "Error in merge"

		
// Define join commit function
let rec jc (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    match lst1 with
    | [] -> if length lst2 = 0 then 
                [] 
            else lst2
    | (Plus,m1)::[] -> 
        (match lst2 with
        | [] -> lst1
        | (Max,l1)::(Join,l2)::xs2 -> 
            if m1 > 1 then 
                jc [(Plus,m1-1)] (seq((Max,l1+l2) :: xs2)) 
            else 
                (Max,l1+l2) :: xs2
         |(Join, l2)::xs2 -> 
            if m1 > 1 then 
                jc [(Plus, m1-1)] (seq ((Max, l2) :: xs2)) 
            else
                (Max, l2) :: xs2
        | otherwise -> failwith "Need attention in jc 1")
    | (Plus,n1)::(Max,n2)::[] -> 
        (match lst2 with
        | [] -> lst1
        | (Max,l1)::(Join,l2)::xs2 -> 
            if n1 >= 1 then 
                jc ((Plus,(n1-1)) :: [Max,(max (n2+1) (l1+l2))]) xs2 
            else 
                jc [Join,max (n2+1) (l1+l2)] xs2
        | (Join, l2)::xs2 -> 
            if n1 > 1 then 
                jc [(Plus, n1-1)] (seq ((Max, l2) :: xs2)) 
            else 
                (Max, max (n2+1) l2) :: xs2
        | otherwise -> failwith "Need attention in jc 2")
    | x::xs -> []
	
// Define choice function
let choice (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    match lst1 with
    | [] -> if length lst2 = 0 then 
                [] 
            else 
                lst2
    | x::xs -> 
        match  lst2 with
        | [] -> lst1
        | x2::xs2 -> 
            if (fst x) = (fst x2) && (xs = xs2) && (fst x) = Max then 
                if (snd x >= snd x2) then 
                    lst1 
                else 
                    lst2
            else []
 