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
let ssa_reverse : (string, string) Hashtbl.t = Hashtbl.create 1023

let currNode : cfg_node ref =
  ref
    {
      label = TAC_Label "";
      comment = TAC_Comment "";
      blocks : tac_instr list = [];
      true_branch : cfg_node option = None;
      false_branch : cfg_node option = None;
      parent_branches : cfg_node option list = [];
    }

let fresh_var () =
  let newVar = sprintf "t$%d" !varCount in
  varCount := !varCount + 1;
  newVar

let fresh_label cname mname =
  let newLabel = sprintf "%s_%s_%d" cname mname !labelCount in
  labelCount := !labelCount + 1;
  newLabel

let tac_type_to_name t =
  match t with
  | IConst _ -> "IConst"
  | BConst _ -> "BConst"
  | SConst _ -> "SConst"
  | Jump _ -> "Jump"
  | Label _ -> "Label"
  | Return _ -> "Return"
  | BranchTrue _ -> "BranchTrue"
  | TAC_Assign_Identifier _ -> "TAC_Assign_Identifier"
  | TAC_Assign_Int _ -> "TAC_Assign_Int"
  | TAC_Assign_String _ -> "TAC_Assign_String"
  | TAC_Assign_Bool _ -> "TAC_Assign_Bool"
  | TAC_Assign_Plus _ -> "TAC_Assign_Plus"
  | TAC_Assign_Minus _ -> "TAC_Assign_Minus"
  | TAC_Assign_Times _ -> "TAC_Assign_Times"
  | TAC_Assign_Div _ -> "TAC_Assign_Div"
  | TAC_Assign_Lt _ -> "TAC_Assign_Lt"
  | TAC_Assign_Le _ -> "TAC_Assign_Le"
  | TAC_Assign_Eq _ -> "TAC_Assign_Eq"
  | TAC_Assign_BoolNegate _ -> "TAC_Assign_BoolNegate"
  | TAC_Assign_ArithNegate _ -> "TAC_Assign_ArithNegate"
  | TAC_Assign_ObjectAlloc _ -> "TAC_Assign_ObjectAlloc"
  | TAC_Assign_ObjectDefault _ -> "TAC_Assign_ObjectDefault"
  | TAC_Assign_NullCheck _ -> "TAC_Assign_NullCheck"
  | TAC_Assign_Dynamic_FunctionCall _ -> "TAC_Assign_Dynamic_FunctionCall"
  | TAC_Assign_Static_FunctionCall _ -> "TAC_Assign_Static_FunctionCall"
  | TAC_Assign_Self_FunctionCall _ -> "TAC_Assign_Self_FunctionCall"
  | TAC_Assign_New _ -> "TAC_Assign_New"
  | TAC_Assign_Default _ -> "TAC_Assign_Default"
  | TAC_Remove_Let _ -> "TAC_Remove_Let"
  | TAC_Assign_Assign _ -> "TAC_Assign_Assign"
  | TAC_Branch_True _ -> "TAC_Branch_True"
  | TAC_Comment _ -> "TAC_Comment"
  | TAC_Label _ -> "TAC_Label"
  | TAC_Jump _ -> "TAC_Jump"
  | TAC_Return _ -> "TAC_Return"
  | TAC_Internal _ -> "TAC_Internal"
  | TAC_Case _ -> "TAC_Case"
  | TAC_End_While _ -> "TAC_End_While"
  | TAC_SSA_Merge _ -> "TAC_SSA_Merge"
  | SSA_Phi _ -> "SSA_Phi"
