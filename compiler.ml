
module StringMap = Map.Make(String)

(* maps variable names in DSL to LLVM registers *)
type name_env = Llvm.llvalue StringMap.t

(* for simple compilers just use one context *)
let context : Llvm.llcontext = Llvm.global_context()

(* annoying to recreate this type repeatedly, so just make it global *)
let f64_t : Llvm.lltype = Llvm.double_type context
(* pre-create these common numbers *)
let zero : Llvm.llvalue = Llvm.const_float f64_t 0.0
let one : Llvm.llvalue = Llvm.const_float f64_t 1.0

(* State which needs to get passed into LLVM functions *)
type llvm_state = {
  builder : Llvm.llbuilder;
  llvm_module : Llvm.llmodule;
  llvm_fn : Llvm.llvalue;
}


let rec compile_exp state names = function
  | Dsl.Num f -> Llvm.const_float f64_t f
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
  let input_types : Llvm.lltype list = List.map (fun _ -> f64_t) f.Dsl.inputs in
  let return_type = f64_t in
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


let compile (f:Dsl.fn) : Llvm.llvalue  =
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
  (* we owe our caller a compiled function *)
  state.llvm_fn



