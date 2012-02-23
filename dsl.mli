type exp =
  | Num of float
  | Var of string
  | Add of exp * exp
  | Sub of exp * exp 
  | Mult of exp * exp
  | Div of exp * exp
  | Sum of string * exp * exp * exp

val exp_to_str : exp -> string

type fn = {name:string; inputs:string list; body: exp}

val fn_to_str : fn -> string
