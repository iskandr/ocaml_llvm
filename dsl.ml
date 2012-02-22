


type exp =
  | Num of float
  | Var of string
  | Mult of exp * exp
  | Div of exp * exp
  | Sum of string * exp * exp * exp

let rec exp_to_str = function
  | Num f -> string_of_float f
  | Var name -> name
  | Mult (x,y) -> Printf.sprintf "(%s * %s)" (exp_to_str x) (exp_to_str y)
  | Div (x,y) -> Printf.sprintf "(%s / %s)" (exp_to_str x) (exp_to_str y)
  | Sum (loop_var, start_val, end_val, body) ->
    Printf.sprintf "sum from %s = %s to %s of %s"
      loop_var
      (exp_to_str start_val)
      (exp_to_str end_val)
      (exp_to_str body)

type fn = {inputs:string list; body: exp}

let fn_to_str {inputs; body} =
  Printf.sprintf "fn(%s) = %s" (String.concat ", " inputs) (exp_to_str body)

