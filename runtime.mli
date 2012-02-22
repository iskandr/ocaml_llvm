
(* the runtime converts OCaml values in LLVM's GenericValues and *)
(* initiates JIT+execution *)
val run : Llvm.llvalue -> float list -> float