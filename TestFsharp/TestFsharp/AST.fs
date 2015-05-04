module AST

open CoreTypeSystem

let ast = Branch ([Leaf (Tag.Plus, 1);  //+1
                  Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); //(+1-1-1-1)
                  Leaf (Tag.Minus, 1);
                  Leaf (Tag.Minus, 1)]) 

printfn "\nTest ast %A" (ast);;

// Pause terminal
let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()
pause ()  