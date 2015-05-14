module BubbleSort

val length: list 'a -> Tot nat
let rec length l = match l with
  | [] -> 0
  | _ :: tl -> 1 + length tl

val swap : l:list int -> Tot (list int) (decreases (length l))
let rec swap l =
    match l with
    |[] -> []
    |x::[] -> x::[]
    |x::y::xs -> if x > y then
                    y::(swap (x::xs))
                else
                    x::(swap (y::xs))

val sort : list int -> count:nat -> Tot (list int) (decreases count)
let rec sort x count = if count > 1 then sort (swap x) (count - 1)
                       else x

val bubleSort : list int -> Tot (list int)
let bubleSort x = sort x (length x)