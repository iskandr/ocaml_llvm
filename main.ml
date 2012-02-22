open Printf
open Dsl


let f = {
  inputs=["k"];
  body=Sum ("i", Num 1.0, Var "k", Div(Num 1.0,  Mult(Var "i", Var "i")))
}

let ocaml_impl stop =
  let rec aux acc i =
    if i > stop then acc
    else aux (acc +. (1.0 /. (i*.i))) (i +. 1.0)
  in
  aux 0.0 1.0

let _ =
  begin
    printf "DSL function:\n";
    printf "\t-- %s\n" (fn_to_str f);
    printf "Compiling...\n";
    let compiled : Compiler.compiled_fn = Compiler.compile f in
    printf "LLVM code:\n";
    Llvm.dump_value compiled.Compiler.fn_val;
    printf "Verifying LLVM function...\n";
    Llvm_analysis.assert_valid_function compiled.Compiler.fn_val;
    let k = 15000000.0 in
    printf "---\nRunning LLVM function:\n";
    (* warm up the JIT *)
    let _ = Compiler.run compiled [1.0] in
    let llvm_start_time = Unix.gettimeofday() in
    let result = Compiler.run compiled [k] in
    let llvm_end_time = Unix.gettimeofday() in
    printf "Time: %f\n" (llvm_end_time -. llvm_start_time);
    printf "Result: %f\n" result;
    printf "---\nRunning OCaml function:\n";
    let ocaml_start_time = Unix.gettimeofday() in
    let ocaml_result = ocaml_impl k in
    let ocaml_end_time = Unix.gettimeofday() in
    printf "Time: %f\n" (ocaml_end_time -. ocaml_start_time);
    printf "Result: %f\n" ocaml_result;

  end
