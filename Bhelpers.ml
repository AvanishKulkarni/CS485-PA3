open Printf
open Atypes

let type_to_str_clean t = match t with Class c | SELF_TYPE c -> c
let tac_expr_to_name t = match t with TAC_Variable c -> c
let varCount = ref 0
let labelCount = ref 1
let stringCounter = ref 0
let divCounter = ref 0
let voidCounter = ref 0
let caseErrorCounter = ref 0
let vtable : (string * string, int) Hashtbl.t = Hashtbl.create 255
let envtable : (string, string) Hashtbl.t = Hashtbl.create 255
let asm_strings : (string, string) Hashtbl.t = Hashtbl.create 255
let attrLocations : (string, string * int) Hashtbl.t = Hashtbl.create 255
let class_tags : (string, int) Hashtbl.t = Hashtbl.create 255
let inheritance : (string, string) Hashtbl.t = Hashtbl.create 255
let ident_tac : (name, tac_expr) Hashtbl.t = Hashtbl.create 255
let dispatch_list : (string * string, bool) Hashtbl.t = Hashtbl.create 255
let ssa_names : (string, int) Hashtbl.t = Hashtbl.create 255

let currNode : cfg_node ref =
  ref
    {
      label = TAC_Label "";
      comment = TAC_Comment "";
      blocks = [];
      true_branch = None;
      false_branch = None;
      parent_branches = [];
    }

let fresh_var () =
  let newVar = sprintf "t$%d" !varCount in
  varCount := !varCount + 1;
  newVar

let fresh_label cname mname =
  let newLabel = sprintf "%s_%s_%d" cname mname !labelCount in
  labelCount := !labelCount + 1;
  newLabel
