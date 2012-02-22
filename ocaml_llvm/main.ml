open Printf
open Dsl


let z = {
  inputs=["k"];
  body=Sum("i", Num 1.0, Var "k", Div(Num 1.0, Mult(Var "i", Var "i")))
}


let _ =
  begin
    printf "DSL function:\n";
    printf "\t-- %s\n" (fn_to_str z);
    printf "Compiling...\n";
    let compiled = Compiler.compile z in
    printf "LLVM code:\n";
    Llvm.dump_value compiled;
    printf "Verifying LLVM function...\n";
    Llvm_analysis.assert_valid_function compiled;
    printf "Running LLVM function...\n";
    let result = Runtime.run compiled [100.0] in
    printf "Result: %f\n" result
  end
