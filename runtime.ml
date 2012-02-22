

module GV = Llvm_executionengine.GenericValue
module LLE = Llvm_executionengine.ExecutionEngine

let _ = Llvm_executionengine.initialize_native_target()



let run f (inputs:float list) : float =
  let llvm_inputs : GV.t list = List.map (GV.of_float Compiler.f64_t) inputs in
  let enclosing_module = Llvm.global_parent f in
  (* create an optimizing JIT (opt-level = 3) *)
  let execution_engine = LLE.create_jit enclosing_module 3 in
  let result : GV.t =
    LLE.run_function f (Array.of_list llvm_inputs) execution_engine

  in
  Llvm_executionengine.GenericValue.as_float Compiler.f64_t result




