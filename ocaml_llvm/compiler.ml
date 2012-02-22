
let context : Llvm.llcontext = Llvm.global_context()
let global_module : Llvm.llmodule = Llvm.create_module context "ForeverAlone"

let f64 : Llvm.lltype = Llvm.double_type context

let compile _ = Llvm.const_float f64 0.0