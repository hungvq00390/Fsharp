module AST

open CoreTypeSystem

type Tree = 
| Branch of Tree list
| Leaf of TagNum 

//+2 (+1-1-1-1)-1-1)
let ast2 = Branch ([Leaf (Tag.Plus, 1); Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1);Leaf (Tag.Minus, 1)]) 


// Assume tree contains one branch of only leaves, no other subbranches
let rec inferLocal (tree: Tree list) : TagSeq =  
    match tree with
        | [Leaf tagnum] -> [tagnum]
        | [Branch []] -> []
        | [Branch t] ->  inferLocal t
        | otherwise -> failwith "A bug in inferLocal."

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
                    let tailseq = seq (merge (prep(child)) (prep(parent))) in
                    seq (jc headseq (prep(tailseq)))

// Pause terminal
let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()
pause ()  