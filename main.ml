open Printf
open Dsl

let f = {
  name = "f";
  inputs=["k"]; 
  body = Sum("i", Num 1.0, Var "k", Div(Num 1.0, Mult(Var "i", Var "i")))
}

let f_ocaml k =
  let rec aux acc i =
    if i > k then acc
    else aux (acc +. (1.0 /. (i*.i))) (i +. 1.0)
  in
  aux 0.0 1.0

let g = {
  name = "g"; 
  inputs=["k"];
  body=Sum ("i", Num 1.0, Var "k", 
          Sum("j", Num 1.0, Var "i", Add(Var "i", Var "j")))
}
let g_ocaml k =
  let sum1 = ref 0.0 in 
  let i = ref 1.0 in 
  while !i < k do
    let sum2 = ref 0.0 in  
    let j = ref 1.0 in 
    while !j < !i do 
      sum2 := !sum2 +. !i +. !j;
      j := !j +. 1.0;
    done;
    sum1 := !sum1 +. !sum2;
    i := !i +. 1.0
  done;
  !sum1 

let compare_functions ?(input=100000.0) (dsl:Dsl.fn) (native:float->float) = 
  printf "DSL function:\n";
  printf "\t-- %s\n" (fn_to_str dsl);
  printf "Compiling...\n";
  let compiled : Compiler.compiled_fn = Compiler.compile dsl in
  printf "---\nRunning LLVM function:\n%!";
  (* warm up the JIT *)
  let _ = Compiler.run compiled [1.0] in
  let llvm_start_time = Unix.gettimeofday() in
  let result = Compiler.run compiled [input] in
  let llvm_end_time = Unix.gettimeofday() in
  printf "Time: %f\n" (llvm_end_time -. llvm_start_time);
  printf "Result: %f\n" result;
  printf "---\nRunning OCaml function:\n%!";
  let ocaml_start_time = Unix.gettimeofday() in
  let ocaml_result = native input in
  let ocaml_end_time = Unix.gettimeofday() in
  printf "Time: %f\n" (ocaml_end_time -. ocaml_start_time);
  printf "Result: %f\n" ocaml_result

let _ = 
  compare_functions f f_ocaml;
  compare_functions ~input:10000.0 g g_ocaml
