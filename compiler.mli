

module StringMap : module type of Map.Make(String)
type name_env = Llvm.llvalue StringMap.t

(* the only datatype our compiler supports  *)
val float_t : Llvm.lltype

(* State which needs to get passed into LLVM functions *)
type llvm_state = {
  builder : Llvm.llbuilder;
  llvm_module : Llvm.llmodule;
  llvm_fn : Llvm.llvalue;
}


val compile_exp : llvm_state -> name_env -> Dsl.exp -> Llvm.llvalue
val init : Dsl.fn -> llvm_state

type compiled_fn = {
  fn_val : Llvm.llvalue;
  execution_engine : Llvm_executionengine.ExecutionEngine.t;
}

val compile : Dsl.fn -> compiled_fn

(* the runtime converts OCaml values in LLVM's GenericValues and *)
(* initiates JIT+execution *)
val run : compiled_fn -> float list -> float
