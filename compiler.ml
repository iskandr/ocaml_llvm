
module StringMap = Map.Make(String)

(* maps variable names in DSL to LLVM registers *)
type name_env = Llvm.llvalue StringMap.t

(* for simple compilers just use one context *)
let context : Llvm.llcontext = Llvm.global_context()

(* annoying to recreate types repeatedly, so just make globals *)
let f32_t : Llvm.lltype = Llvm.float_type context

(* pre-create these common numbers *)
let zero : Llvm.llvalue = Llvm.const_float f32_t 0.0
let one : Llvm.llvalue = Llvm.const_float f32_t 1.0

(* State which needs to get passed into LLVM functions *)
type llvm_state = {
  builder : Llvm.llbuilder;
  llvm_module : Llvm.llmodule;
  llvm_fn : Llvm.llvalue;
}


let rec compile_exp state names = function
  | Dsl.Num f -> Llvm.const_float f32_t f
  | Dsl.Var x ->
    if StringMap.mem x names then StringMap.find x names
    else failwith ("Undefined variable " ^ x)
  | Dsl.Div (x,y) ->
    let x' : Llvm.llvalue = compile_exp state names x in
    let y' : Llvm.llvalue = compile_exp state names y in
    Llvm.build_fdiv x' y' "div_result" state.builder
  | Dsl.Mult (x,y) ->
    let x' : Llvm.llvalue = compile_exp state names x in
    let y' : Llvm.llvalue = compile_exp state names y in
    Llvm.build_fmul x' y' "mult_result" state.builder
  | Dsl.Sum(loop_var_name, start, stop, body) ->
    let start' : Llvm.llvalue =  compile_exp state names start in
    let stop' : Llvm.llvalue = compile_exp state names stop in
    (* what block are we currently inserting into *)
    let curr_block : Llvm.llbasicblock = Llvm.insertion_block state.builder in
    compile_loop state names curr_block loop_var_name start' stop' body

and compile_loop state names old_block loop_var_name start stop body =
    (* create a loop header where we test whether the loop should continue *)
    let loop_header : Llvm.llbasicblock =
      Llvm.append_block context "loop_header" state.llvm_fn
    in
    (* make the original code jump into the loop header we've built *)
    let _ = Llvm.build_br loop_header state.builder in
    (* move to the loop header we just created *)
    Llvm.position_at_end loop_header state.builder;
    (* initially the phi node only knows about what happened before the loop, *)
    (* we'll add another incoming edge later *)
    (* To start, we initialize the result to zero *)
    let result : Llvm.llvalue =
      Llvm.build_phi [zero, old_block] "result" state.builder
    in
    (* also initialize the loop variable to whatever the program specifies *)
    let loop_var : Llvm.llvalue =
      Llvm.build_phi [start, old_block] loop_var_name state.builder
    in

    (* The module Llvm.Fcmp contains two sorts of comparisons. *)
    (* Ordered comparisons, such as Ogt (ordered greater than) will be false*)
    (* if an argument is NaN. Unordered don't care. *)
    let cond =
      Llvm.build_fcmp Llvm.Fcmp.Ogt loop_var stop "loop_cond" state.builder
    in
    (* create another block for the loop body itself *)
    let loop_body : Llvm.llbasicblock =
      Llvm.append_block context "loop_body" state.llvm_fn
    in
    let after_loop : Llvm.llbasicblock =
      Llvm.append_block context "after_loop" state.llvm_fn
    in
    (*  check whether the loop_var exceeds the end_val, if so jump back    *)
    let _ = Llvm.build_cond_br cond after_loop loop_body state.builder in
    (* move the builder to the loop body *)
    Llvm.position_at_end loop_body state.builder;
    (* add the loop variable to the name environment *)
    let names' : name_env = StringMap.add loop_var_name loop_var names in
    let curr_val : Llvm.llvalue = compile_exp state names' body in
    let next_result : Llvm.llvalue =
      Llvm.build_fadd result curr_val "next_result" state.builder
    in
    (* add an edge to the phi node so that result and next_result are merged *)
    Llvm.add_incoming (next_result, loop_body) result;
    let next_loop_var =
      Llvm.build_fadd loop_var one "next_loop_var" state.builder
    in
    (* update the phi node for the loop var also *)
    Llvm.add_incoming (next_loop_var, loop_body) loop_var;
    let _ = Llvm.build_br loop_header state.builder in
    (* move builder back to end of the block we were building*)
    Llvm.position_at_end  after_loop state.builder;
    result

let init (f : Dsl.fn) : llvm_state =
  (* for now modules aren't really used but still need to exist *)
  let m : Llvm.llmodule = Llvm.create_module context "M" in
  let input_types : Llvm.lltype list = List.map (fun _ -> f32_t) f.Dsl.inputs in
  let return_type = f32_t in
  let fn_type : Llvm.lltype =
    Llvm.function_type return_type (Array.of_list input_types)
  in
  (* make a fresh function which takes some float64's and returns a float64 *)
  let llvm_fn : Llvm.llvalue = Llvm.declare_function "fn" fn_type m in
  let builder : Llvm.llbuilder  = Llvm.builder context in
  (* create an entry block to the function and move our builder to this block *)
  let entry : Llvm.llbasicblock = Llvm.append_block context "entry" llvm_fn in
  Llvm.position_at_end entry builder;
  { builder = builder; llvm_module = m; llvm_fn = llvm_fn }


module LLE = Llvm_executionengine.ExecutionEngine


let optimize llvm_fn llvm_module execution_engine =
  let pm = Llvm.PassManager.create_function llvm_module in
  (* Set up the optimizer pipeline.  Start with registering info about how the
  * target lays out data structures. *)
  Llvm_target.TargetData.add (LLE.target_data execution_engine) pm;

  (* THROW EVERY OPTIMIZATION UNDER THE SUN AT THE CODE *)
  Llvm_scalar_opts.add_licm pm;
  Llvm_scalar_opts.add_loop_deletion pm;
  Llvm_scalar_opts.add_loop_rotation pm;
  Llvm_scalar_opts.add_loop_idiom pm;
  Llvm_scalar_opts.add_loop_unswitch pm;
  Llvm_scalar_opts.add_loop_unroll pm;


  Llvm_scalar_opts.add_memory_to_register_promotion pm;
  Llvm_scalar_opts.add_sccp pm;
  Llvm_scalar_opts.add_aggressive_dce pm;
  Llvm_scalar_opts.add_instruction_combination pm;
  Llvm_scalar_opts.add_cfg_simplification pm;

  Llvm_scalar_opts.add_scalar_repl_aggregation pm;
  Llvm_scalar_opts.add_reassociation pm;
  Llvm_scalar_opts.add_jump_threading pm;
  Llvm_scalar_opts.add_lower_expect_intrinsic pm;

  Llvm_scalar_opts.add_basic_alias_analysis pm;
  Llvm_scalar_opts.add_type_based_alias_analysis pm;
  Llvm_scalar_opts.add_ind_var_simplification pm;
  Llvm_scalar_opts.add_dead_store_elimination pm;
  Llvm_scalar_opts.add_memcpy_opt pm;
  Llvm_scalar_opts.add_gvn pm;
  Llvm_scalar_opts.add_correlated_value_propagation pm;



  ignore (Llvm.PassManager.run_function llvm_fn pm);
  ignore (Llvm.PassManager.finalize pm);
  Llvm.PassManager.dispose pm


type compiled_fn = {
  fn_val : Llvm.llvalue;
  execution_engine : LLE.t;
}


let compile (f:Dsl.fn) : compiled_fn  =
  (* initialize an empty function *)
  let state : llvm_state = init f in
  (* grabs the registers which store inputs *)
  let llvm_inputs : Llvm.llvalue array = Llvm.params state.llvm_fn in
  (* ...and combine them with the names of variables to make an env *)
  let names =
    List.fold_left2
      (fun env name llvm_var -> StringMap.add name llvm_var env)
      StringMap.empty
      f.Dsl.inputs
      (Array.to_list llvm_inputs)
  in
  let result = compile_exp state names f.Dsl.body in
  (* return the last value *)
  let _  = Llvm.build_ret result state.builder in

  (* create an optimizing JIT (opt-level = 3) *)
  let execution_engine : LLE.t =
    Llvm_executionengine.ExecutionEngine.create_jit state.llvm_module 3
  in
  optimize state.llvm_fn state.llvm_module execution_engine;
  (* return the function associated with the execution engine *)
  {
    fn_val = state.llvm_fn;
    execution_engine = execution_engine
  }



(* make sure we can compile native code *)
let _ = Llvm_executionengine.initialize_native_target()

module GV = Llvm_executionengine.GenericValue


let run (f:compiled_fn) (inputs:float list) : float =
  let llvm_inputs : GV.t list = List.map (GV.of_float f32_t) inputs in
  let result : GV.t =
    LLE.run_function f.fn_val (Array.of_list llvm_inputs) f.execution_engine
  in
  GV.as_float f32_t result





