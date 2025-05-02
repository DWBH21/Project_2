(** MiniML compiler. *)

open Machine

(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile {Zoo.data=e'; _} =
  match e' with
    | Syntax.Var x -> [IVar x]
    | Syntax.Int k -> [IInt k]
    | Syntax.Bool b -> [IBool b]
    | Syntax.Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Syntax.Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Syntax.Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Syntax.Equal (e1, e2) -> (compile e1) @ (compile e2) @ [IEqual]
    | Syntax.Less (e1, e2) -> (compile e1) @ (compile e2) @ [ILess]
    | Syntax.If (e1, e2, e3) -> (compile e1) @ [IBranch (compile e2, compile e3)]
    | Syntax.Fun (f, x, _, _, e) -> [IClosure (f, x, compile e @ [IPopEnv])]
    | Syntax.Apply (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
    | Syntax.Divide (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]
    
    (* These recursively break the expression into simpler expressions using pattern matching and then call the compile function recursively. *)
    (* All the resulting frames (list of instructions) are then concatenated into a frame a single list of instructions *)
    | Syntax.TryWith (e1, handlers) -> (compile e1) @ [ IHandle ( List.map (fun (ex , e2) -> (ex, compile(e2)) ) handlers ) ] 
    | Syntax.Exception (ex) -> [IExp ex]
    | Syntax.Raise (ex) -> [IRaise ex]
  
