module AST

open CoreTypeSystem

type Tree = 
    | Branch of Tree list
    | Leaf of TagNum 

let rec infer (branch: Tree list) (headseq:TagNum list) = 
    match branch with 
    | [] -> seq headseq
    | x::xs -> 
        match x with
            | Leaf tagnum -> 
                let newhead = seq (List.append headseq [tagnum]) in
                    infer xs newhead
            | Branch br -> 
                let child = join (infer br []) in 
                let parent = join (infer xs []) in
                let tailseq = seq (merge child parent) in
                seq (jc headseq tailseq)

let rec addTree (lst1: char list) : Tree list =
    match lst1 with
    | [] -> []
    | '-'::xs -> List.append [Leaf (Tag.Minus, 1)] (addTree xs)
    | '+'::xs -> List.append [Leaf (Tag.Plus, 1)] (addTree xs)
    | '('::xs -> [Branch (addTree xs)]
    | ')'::xs -> addTree xs
    | x::xs -> addTree xs

System.Console.ReadKey(true) |> ignore  