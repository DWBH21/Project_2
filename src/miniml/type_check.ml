(** Type checking. *)

open Syntax
exception Typing_Error (* Creating a new user defined exception *)

let typing_error ~loc = Zoo.error ~kind:"Type error" ~loc

(** [check ctx ty e] verifies that expression [e] has type [ty] in
    context [ctx]. If it does, it returns unit, otherwise it raises the
    [Type_error] exception. *)
let rec check ctx ty (e) =
  let ty' = type_of ctx e in
    if ty' <> ty then
      raise Typing_Error        (* Throwing an exception instead of the usual error message *)
        (* "This expression has type %t but is used as if it has type %t"
        (Print.ty ty')
        (Print.ty ty) *)

(** [type_of ctx e] computes the type of expression [e] in context
    [ctx]. If [e] does not have a type it raises the [Type_error]
    exception. *)
and type_of ctx {Zoo.data=e; loc} =
  match e with
    | Var x ->
      (try List.assoc x ctx with
	  Not_found -> typing_error ~loc "unknown variable %s" x)
    | Int _ -> TInt
    | Bool _ -> TBool

    (* Modifying all branches to catch a Typing_Error exception *)
    | Exception _ -> TExp (* An exception value is of type TExp itself*)
    | Raise _ -> TExp   (* A raised exception is of type TExp *)
    | Times (e1, e2) -> (try check ctx TInt e1 ; check ctx TInt e2 ; TInt with Typing_Error -> TExp)    
    | Divide (e1, e2) -> (try check ctx TInt e1 ; check ctx TInt e2; TInt with Typing_Error -> TExp)
    | Plus (e1, e2) -> (try check ctx TInt e1 ; check ctx TInt e2 ; TInt with Typing_Error -> TExp)
    | Minus (e1, e2) -> (try check ctx TInt e1 ; check ctx TInt e2 ; TInt with Typing_Error -> TExp)
    | Equal (e1, e2) -> (try check ctx TInt e1 ; check ctx TInt e2 ; TBool with Typing_Error -> TExp)
    | Less (e1, e2) -> (try check ctx TInt e1 ; check ctx TInt e2 ; TBool with Typing_Error -> TExp)
    | If (e1, e2, e3) ->
      (try check ctx TBool e1 ;
      let ty = type_of ctx e2 in
	check ctx ty e3 ; ty with Typing_Error -> TExp)

    | Fun (f, x, ty1, ty2, e) ->
      (try check ((f, TArrow(ty1,ty2)) :: (x, ty1) :: ctx) ty2 e ;
      TArrow (ty1, ty2) with Typing_Error -> TExp)

    (* Checking if the try expression is correct within its context and then checks if all the handlers have same type*)
    | TryWith (e1, handlers) -> 
      let ty_e1 = type_of ctx e1 in 
      let rec check_handlers handlers =
        match handlers with 
        | [] -> ()
        | (_ , e2) :: rest ->
           check ctx ty_e1 e2; check_handlers rest
        in 
      (try check_handlers handlers; ty_e1 with Typing_Error -> TExp)    (* returns the type of the try_expression which is also common to all the exception handlers*)
      
      | Apply (e1, e2) ->
      (try begin match type_of ctx e1 with
	  TArrow (ty1, ty2) -> check ctx ty1 e2 ; ty2
	  | ty ->
	  typing_error ~loc
            "this expression is used as a function but its type is %t" (Print.ty ty)
    end
  with Typing_Error -> TExp)