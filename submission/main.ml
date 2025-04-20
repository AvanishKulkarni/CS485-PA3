(* Allen Cabrera, Avanish Kulkarni - PA3 *)

open Printf

type static_type =
  | Class of string (* ex "Int" or "Object" *)
  | SELF_TYPE of string
let type_to_str_clean t = match t with Class c | SELF_TYPE c -> c

type cool_program = cool_class list
and loc = int
and name = string
and id = loc * name
and cool_type = id
and cool_class = id * id option * feature list

and feature =
  | Attribute of id * cool_type * exp option
  | Method of id * formal list * cool_type * exp

and formal = id * cool_type

and exp = {
  loc : loc;
  exp_kind : exp_kind;
  static_type : static_type option;
}

and exp_kind =
  | Assign of id * exp 
  | Dynamic_Dispatch of exp * id * exp list
  | Static_Dispatch of exp * id * id * exp list
  | Self_Dispatch of id * exp list
  | If of exp * exp * exp
  | While of exp * exp
  | Block of exp list
  | New of id
  | Isvoid of exp
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Divide of exp * exp
  | Lt of exp * exp
  | Le of exp * exp
  | Eq of exp * exp
  | Not of exp
  | Negate of exp
  | Integer of int
  | String of string
  | Identifier of id
  | Bool of string (* bool *)
  | Let of binding list * exp
  | Case of exp * case_elem list
  | Internal of
      string
      * string
      * string (* return class, class its defined in, method name *)

and binding = Binding of id * cool_type * exp option
and case_elem = Case_Elem of id * cool_type * exp

type tac_instr =
  | IConst of iconst
  | BConst of bconst
  | SConst of sconst 
  | Jump of label
  | Label of label
  | Return of string
  | BranchTrue of bconst * label
  | TAC_Assign_Identifier of label * label
  | TAC_Assign_Int of label * iconst
  | TAC_Assign_String of label * sconst
  | TAC_Assign_Bool of label * bconst
  | TAC_Assign_Plus of label * tac_expr * tac_expr
  | TAC_Assign_Minus of label * tac_expr * tac_expr
  | TAC_Assign_Times of label * tac_expr * tac_expr
  | TAC_Assign_Div of label * tac_expr * tac_expr
  | TAC_Assign_Lt of label * tac_expr * tac_expr
  | TAC_Assign_Le of label * tac_expr * tac_expr
  | TAC_Assign_Eq of label * tac_expr * tac_expr
  | TAC_Assign_BoolNegate of label * tac_expr
  | TAC_Assign_ArithNegate of label * tac_expr
  | TAC_Assign_ObjectAlloc of label * label (* might have to change to tac_expr *)
  | TAC_Assign_ObjectDefault of label * label
  | TAC_Assign_NullCheck of label * tac_expr
  | TAC_Assign_Dynamic_FunctionCall of label * label * label * (tac_expr list)
  | TAC_Assign_Static_FunctionCall of label * label * label * (tac_expr list)
  | TAC_Assign_Self_FunctionCall of label * label * label * (tac_expr list)
  | TAC_Assign_New of label * label
  | TAC_Assign_Default of label * label
  | TAC_Remove_Let of label
  | TAC_Assign_Assign of label * tac_expr
  | TAC_Branch_True of bconst * label
  | TAC_Comment of string
  | TAC_Label of label
  | TAC_Jump of label
  | TAC_Return of label
  | TAC_Internal of label 
  | TAC_Case of label * label * case_elem list * (tac_instr list list)
and tac_expr =
  | TAC_Variable of label
and label = string
and iconst = string
and bconst = string 
and sconst = string
and cfg_node = {
  label: tac_instr;
  comment: tac_instr;
  mutable blocks: tac_instr list;
  mutable true_branch: cfg_node option;
  mutable false_branch: cfg_node option;
  mutable parent_branches: cfg_node option list;
}

let tac_expr_to_name t = match t with TAC_Variable c -> c
let varCount = ref 0;;
let labelCount = ref 1;;

let stringCounter = ref 0;;
let divCounter = ref 0;;
let voidCounter = ref 0;;
let caseErrorCounter = ref 0;;

let vtable : ((string * string), int) Hashtbl.t = Hashtbl.create 255
let envtable : (string, string) Hashtbl.t = Hashtbl.create 255
let asm_strings : (string, string) Hashtbl.t = Hashtbl.create 255 
let attrLocations : (string, string * int) Hashtbl.t = Hashtbl.create 255
let class_tags : (string, int) Hashtbl.t = Hashtbl.create 255

let inheritance : (string, string) Hashtbl.t = Hashtbl.create 255

let main() = (
  Printexc.record_backtrace true;
  let fname = Sys.argv.(1) in 
  let fin = open_in fname in 
  let rec range k = if k <= 0 then [] else k :: range (k - 1) in
  let read () = input_line fin in 
  let read_list worker =
    let k = int_of_string (read ()) in
    let lst = range k in
    List.map (fun _ -> worker ()) lst
  in

  (* deserialize class map *)
  let rec read_class_map () = (
    let _ = read () in (* read "class_map" and skip *)
    let classes = read_list read_class_class_map in 
    (classes)
  ) 
  and read_class_class_map () = (
    let cname = read () in 
    let attrs = read_list read_attr in 
    (cname, attrs)
  )
  and read_attr () = (
    let init = read () in 
    let aname = read () in 
    let atype = read () in 
    let aexp = match init with
    | "no_initializer" -> None
    | "initializer" -> Some(read_exp ()) 
    | _ -> failwith "attr read failed in read_class_map"
  in (aname, atype, aexp)
  )
  and read_impl_map () = (
    let _ = read () in (* read "implementation_map" *)
    let classes = read_list read_class_imp_map
  in (classes)
  ) 
  and read_class_imp_map () = (
    let cname = read () in  
    let methods = read_list read_method in 
    (cname, methods)
  )
  and read_method () = (
    let mname = read () in 
    let formals = read_list read in 
    let parent = read () in 
    let mbody = read_exp () in 
    (mname, formals, parent, mbody)
  )
  and read_parent_map () = (
    let _ = read () in (* read "parent_map" *)
    let relations = read_list read_relations in 
    (relations)
  ) 
  and read_relations () = (
    let child = read () in 
    let parent = read () in 
    (child, parent)
  )
  (* TODO: implement reading the annotated AST from PA2 *)
  and read_cool_program () = read_list read_cool_class
  and read_id () =
    let loc = int_of_string (read ()) in
    let name = read () in
    (loc, name)
  and read_cool_class () =
    (* Class *)
    let cname = read_id () in
    let inherits =
      match read () with
      | "no_inherits" -> None
      | "inherits" ->
          let super = read_id () in
          Some super
      | x -> failwith ("cannot happen: " ^ x)
    in
    let features = read_list read_feature in
    (cname, inherits, features)
  and read_feature () =
    match read () with
    | "attribute_no_init" ->
        let fname = read_id () in
        let ftype = read_id () in
        Attribute (fname, ftype, None)
    | "attribute_init" ->
        let fname = read_id () in
        let ftype = read_id () in
        let finit = read_exp () in
        Attribute (fname, ftype, Some finit)
    | "method" ->
        let mname = read_id () in
        let formals = read_list read_formal in
        let mtype = read_id () in
        let mbody = read_exp () in
        Method (mname, formals, mtype, mbody)
    | x -> failwith ("cannot happen: " ^ x)
  and read_formal () =
    let fname = read_id () in
    let ftype = read_id () in
    (fname, ftype)
  and read_let_binding () =
    match read () with
    | "let_binding_no_init" ->
        let letvar = read_id () in
        let lettype = read_id () in
        Binding (letvar, lettype, None)
    | "let_binding_init" ->
        let letvar = read_id () in
        let lettype = read_id () in
        let letval = read_exp () in
        Binding (letvar, lettype, Some letval)
    | x -> failwith ("impossible binding " ^ x)
  and read_case_elem () =
    let csid = read_id () in
    let cstype = read_id () in
    let csbody = read_exp () in
    Case_Elem (csid, cstype, csbody)
  and read_exp () =
    let eloc = int_of_string (read ()) in
    let cool_type = read () in 
    let ekind =
      match read () with
      (* do the rest of the types *)
      | "assign" ->
          let avar = read_id () in
          let aexp = read_exp () in
          Assign (avar, aexp)
      | "dynamic_dispatch" ->
          let ddexp = read_exp () in
          let ddmethod = read_id () in
          let ddargs = read_list read_exp in
          Dynamic_Dispatch (ddexp, ddmethod, ddargs)
      | "static_dispatch" ->
          let sdexp = read_exp () in
          let sdid = read_id () in
          let sdmethod = read_id () in
          let sdargs = read_list read_exp in
          Static_Dispatch (sdexp, sdid, sdmethod, sdargs)
      | "self_dispatch" ->
          let sdmethod = read_id () in
          let sdargs = read_list read_exp in
          Self_Dispatch (sdmethod, sdargs)
      | "if" ->
          let ipred = read_exp () in
          let ithen = read_exp () in
          let ielse = read_exp () in
          If (ipred, ithen, ielse)
      | "while" ->
          let wpred = read_exp () in
          let wbody = read_exp () in
          While (wpred, wbody)
      | "block" ->
          let bbody = read_list read_exp in
          Block bbody
      | "new" ->
          let nclass = read_id () in
          New nclass
      | "isvoid" ->
          let ivexp = read_exp () in
          Isvoid ivexp
      | "plus" ->
          let x = read_exp () in
          let y = read_exp () in
          Plus (x, y)
      | "minus" ->
          let x = read_exp () in
          let y = read_exp () in
          Minus (x, y)
      | "times" ->
          let x = read_exp () in
          let y = read_exp () in
          Times (x, y)
      | "divide" ->
          let x = read_exp () in
          let y = read_exp () in
          Divide (x, y)
      | "lt" ->
          let x = read_exp () in
          let y = read_exp () in
          Lt (x, y)
      | "le" ->
          let x = read_exp () in
          let y = read_exp () in
          Le (x, y)
      | "eq" ->
          let x = read_exp () in
          let y = read_exp () in
          Eq (x, y)
      | "not" ->
          let x = read_exp () in
          Not x
      | "negate" ->
          let x = read_exp () in
          Negate x
      | "integer" ->
          let ival = read () in
          Integer (int_of_string ival)
      | "string" ->
          let sval = read () in
          String sval
      | "identifier" ->
          let idvar = read_id () in
          Identifier idvar
      | "true" ->
          let bval = "true" in
          Bool bval
      | "false" ->
          let bval = "false" in
          Bool bval
      | "let" ->
          let letbinding = read_list read_let_binding in
          let letbody = read_exp () in
          Let (letbinding, letbody)
      | "case" ->
          let csexp = read_exp () in
          let cselemlist = read_list read_case_elem in
          Case (csexp, cselemlist)
      | "internal" -> 
          let name = read () in 
          Internal (name, name, name) (* this is definitely wrong but im ignoring it *)
      | x -> failwith ("invalid expression kind: " ^ x)
    in
    { loc = eloc; exp_kind = ekind; static_type = Some(Class(cool_type)) }
  in
  let rec read_cltype () = 
    let class_map = read_class_map () in
    let impl_map = read_impl_map () in
    let parent_map = read_parent_map () in
    let ast = read_cool_program () in 
    (class_map, impl_map, parent_map, ast)
  in 

  let cltype = read_cltype () in 

  (* TODO: return a unique variable name *)
  let fresh_var () = (
    let newVar = 
    sprintf "t$%d" !varCount
    in
    varCount := !varCount +1;
    newVar
  ) in
  let fresh_label cname mname = (
    let newLabel = 
      (sprintf "%s_%s_%d" cname mname !labelCount) in labelCount := !labelCount + 1;
      newLabel
  ) in
  let currNode : cfg_node ref = ref {
    label = TAC_Label("");
    comment = TAC_Comment("");
    blocks = [];
    true_branch = None;
    false_branch = None;
    parent_branches = [];
  } in
  let rec merge_node curr_node join_node = (
    match curr_node with
    | Some(curr_node) -> (
      match curr_node.true_branch with
      | None -> curr_node.true_branch <- join_node;
      | Some(node_branch) -> merge_node (Some(node_branch)) join_node;
    )
    | None -> ();
  )
    in
  let ident_tac : (name, (tac_expr)) Hashtbl.t = Hashtbl.create 255 in
  let rec convert (a: exp_kind) (var : name) (cname: name) (mname: name) : (tac_instr list * tac_expr) = (
    match a with
      | Identifier(v) -> 
        let _, name = v in
        (* printf "%s\n" name; *)
        (match Hashtbl.find_opt ident_tac name with 
        | Some(ta) -> 
          (* printf "found -> returning %s\n" (tac_expr_to_name ta); *)
          !currNode.blocks <- !currNode.blocks @ [TAC_Assign_Identifier(var, (tac_expr_to_name ta))];
          [TAC_Assign_Identifier(var, (tac_expr_to_name ta))], TAC_Variable(var)
        | None -> 
          (* printf "created %s \n" name; *)
          Hashtbl.add ident_tac name (TAC_Variable(name));
          !currNode.blocks <- !currNode.blocks @ [TAC_Assign_Identifier(var, name)];
          [TAC_Assign_Identifier(var, name)], TAC_Variable(var))
      | Integer(i) ->
        !currNode.blocks <- !currNode.blocks @ [TAC_Assign_Int(var, string_of_int i)];
        [TAC_Assign_Int(var, string_of_int i)], (TAC_Variable(var))
      | Bool(i) ->
        !currNode.blocks <- !currNode.blocks @ [TAC_Assign_Bool(var, i)];
        [TAC_Assign_Bool(var, i)], (TAC_Variable(var))
      | String(i) ->
        !currNode.blocks <- !currNode.blocks @ [TAC_Assign_String(var, i)];
        [TAC_Assign_String(var, i)], (TAC_Variable(var))
      | Plus(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 cname mname in
        let i2, ta2 = convert a2.exp_kind arg2 cname mname in
        let to_output = TAC_Assign_Plus(var, ta1, ta2) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Minus(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 cname mname in
        let i2, ta2 = convert a2.exp_kind arg2 cname mname in
        let to_output = TAC_Assign_Minus(var, ta1, ta2) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Times(a1, a2) -> 
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 cname mname in
        let i2, ta2 = convert a2.exp_kind arg2 cname mname in
        let to_output = TAC_Assign_Times(var, ta1, ta2) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Divide(a1, a2) -> 
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 cname mname in
        let i2, ta2 = convert a2.exp_kind arg2 cname mname in
        let to_output = TAC_Assign_Div(var, ta1, ta2) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        Hashtbl.add asm_strings ("ERROR: " ^ string_of_int(a1.loc) ^ ": Exception: division by zero\\n") ("divErrString" ^ string_of_int(!divCounter));
        divCounter := !divCounter + 1;
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Lt(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 cname mname in
        let i2, ta2 = convert a2.exp_kind arg2 cname mname in
        let to_output = TAC_Assign_Lt(var, ta1, ta2) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Le(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 cname mname in
        let i2, ta2 = convert a2.exp_kind arg2 cname mname in
        let to_output = TAC_Assign_Le(var, ta1, ta2) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Eq(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 cname mname in
        let i2, ta2 = convert a2.exp_kind arg2 cname mname in
        let to_output = TAC_Assign_Eq(var, ta1, ta2) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Not(a1) ->
        let i1, ta1 = convert a1.exp_kind (fresh_var()) cname mname in
        let to_output = TAC_Assign_BoolNegate(var, ta1) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ [to_output]), (TAC_Variable(var))
      | Negate(a1) ->
        let i1, ta1 = convert a1.exp_kind (fresh_var()) cname mname in
        let to_output = TAC_Assign_ArithNegate(var, ta1) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ [to_output]), (TAC_Variable(var))
      | Isvoid(a1) ->
        let i1, ta1 = convert a1.exp_kind (fresh_var()) cname mname in
        let to_output = TAC_Assign_NullCheck(var, ta1) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (i1 @ [to_output]), (TAC_Variable(var))
      | Block(exp) ->
        let retTacInstr = ref [] in
        let last_statement = List.hd (List.rev exp) in
        let rest_of_list = List.rev(List.tl (List.rev exp)) in
        let propagateReturn = ref [] in
        List.iter( fun e ->
          let i1, ta1 = convert e.exp_kind (fresh_var()) cname mname in
          retTacInstr := List.append !retTacInstr i1;
          propagateReturn := !propagateReturn @ [TAC_Remove_Let(tac_expr_to_name ta1)];
        ) rest_of_list;
        let i1, _ = convert last_statement.exp_kind var cname mname in
        retTacInstr := List.append !retTacInstr i1;
        !currNode.blocks <- !currNode.blocks @ !propagateReturn;
        (!retTacInstr @ !propagateReturn), (TAC_Variable(var))
      | Dynamic_Dispatch(caller, (_, mname), args) ->
        let retTacInstr = ref [] in
        let args_vars = ref [] in
        List.iter(fun a ->
          let i, ta = convert a.exp_kind (fresh_var()) cname mname in
          retTacInstr := List.append !retTacInstr i;
          args_vars := List.append !args_vars [ta]
        ) args;
        let i, ta = convert caller.exp_kind (fresh_var ()) cname mname in
        let callerType = match caller.static_type with
        | Some(stype) -> type_to_str_clean stype;
        | None -> "";
        in
        let callerType =
          match callerType with
            | "SELF_TYPE" -> cname
            | _ -> callerType
        in
        let to_output = TAC_Assign_Dynamic_FunctionCall(var, mname, callerType, !args_vars) in
        Hashtbl.add asm_strings ("ERROR: " ^ string_of_int(caller.loc) ^ ": Exception: dispatch on void\\n") ("voidErrString" ^ string_of_int(!voidCounter));
        voidCounter := !voidCounter + 1;
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (!retTacInstr @ i @ [to_output]), TAC_Variable(var)
      | Self_Dispatch((_,mname), args) -> 
        let retTacInstr = ref [] in
        let args_vars = ref [] in
        List.iter(fun a ->
          let i, ta = convert a.exp_kind (fresh_var()) cname mname in
          retTacInstr := List.append !retTacInstr i;
          args_vars := List.append !args_vars [ta]
        ) args;
        let to_output = TAC_Assign_Self_FunctionCall(var, mname, cname, !args_vars) in
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (!retTacInstr @ [to_output]), TAC_Variable(var)
      | Static_Dispatch(caller, (_, stype), (_, mname), args) ->
        let retTacInstr = ref [] in
        let args_vars = ref [] in
        List.iter(fun a ->
          let i, ta = convert a.exp_kind (fresh_var()) cname mname in
          retTacInstr := List.append !retTacInstr i;
          args_vars := List.append !args_vars [ta]
        ) args;
        let i, ta = convert caller.exp_kind (fresh_var ()) cname mname in
        let to_output = TAC_Assign_Static_FunctionCall(var, mname, stype, !args_vars) in
        Hashtbl.add asm_strings ("ERROR: " ^ string_of_int(caller.loc) ^ ": Exception: static dispatch on void\\n") ("voidErrString" ^ string_of_int(!voidCounter));
        voidCounter := !voidCounter + 1;
        !currNode.blocks <- !currNode.blocks @ [to_output];
        (!retTacInstr @ i @ [to_output]), TAC_Variable(var)
      | New((_, name)) ->
        let name =
          match name with
            | "SELF_TYPE" -> cname
            | _ -> name
        in
        !currNode.blocks <- !currNode.blocks @ [TAC_Assign_New(var, name)];
        [TAC_Assign_New(var, name)], TAC_Variable(var)
      | Let(bindlist, let_body) ->
        let retTacInstr = ref [] in
        let let_vars = ref [] in
        let removeScope = ref [] in
        List.iteri
            (fun iter (Binding ((vloc, vname), (_, typename), binit)) ->
              match binit with
              (* [Let-Init] *)
              | Some binit ->
                let var = fresh_var () in
                Hashtbl.add ident_tac vname (TAC_Variable(var));
                let i, ta = convert binit.exp_kind (var) cname mname in
                retTacInstr := List.append !retTacInstr [TAC_Assign_Assign(var, ta)];
                !currNode.blocks <- !currNode.blocks @ [TAC_Assign_Identifier(var, tac_expr_to_name ta)];
                let_vars := List.append !let_vars [TAC_Variable(var)];
                removeScope := List.append !removeScope [TAC_Remove_Let(var)];
              (* [Let-No-Init] *)
              | None -> 
                let var = fresh_var () in
                Hashtbl.add ident_tac vname (TAC_Variable(var));
                retTacInstr := List.append !retTacInstr [TAC_Assign_Default(var, typename)];
                !currNode.blocks <- !currNode.blocks @ [TAC_Assign_Default(var, typename)];
                let_vars := List.append !let_vars [TAC_Variable(var)];
                removeScope := List.append !removeScope [TAC_Remove_Let(var)];
              )
            bindlist;
        let i, ta = convert let_body.exp_kind var cname mname in
        List.iter
            (fun (Binding ((_, vname), (_, _), _)) ->
              Hashtbl.remove ident_tac vname;
            )
            bindlist;
        !currNode.blocks <- !currNode.blocks @ !removeScope;
        (!retTacInstr @ i @ !removeScope), TAC_Variable(var)
      | Assign((_, name), exp) ->
        let tac_var = Hashtbl.find ident_tac name in
        (* Hashtbl.add ident_tac name (TAC_Variable(var)); *)
        let i, ta = convert exp.exp_kind (fresh_var ())cname mname in
        let new_var = var in
        let new_id = TAC_Assign_Identifier(new_var, (tac_expr_to_name tac_var)) in
        let to_output = TAC_Assign_Assign((tac_expr_to_name tac_var), ta) in
        !currNode.blocks <- !currNode.blocks @ [to_output] @ [new_id];
        (i @ [to_output] @ [new_id]), (TAC_Variable(new_var))
      (* Need to finish rest of tac for objects and conditionals*)
      | If (pred, astthen, astelse) -> 
        let thenvar = fresh_var () in 
        let thenlbl = fresh_label cname mname in 
        (* let elsevar = fresh_var () in  *)
        let elselbl = fresh_label cname mname in 
        let joinlbl = fresh_label cname mname in 
        let pinstr, pexp = convert pred.exp_kind (thenvar) cname mname in 
        (* let notc = TAC_Assign_BoolNegate(elsevar, pexp) in *)
        (* let bt = TAC_Branch_True(thenvar, thenlbl) in *)
        let be = TAC_Branch_True((tac_expr_to_name pexp), elselbl) in 
        let tcomm = TAC_Comment("then branch") in 
        let tlbl = TAC_Label(thenlbl) in
        let ecomm = TAC_Comment("else branch") in 
        let elbl = TAC_Label(elselbl) in 
        let jcomm = TAC_Comment(sprintf "if-join %s-%s" thenlbl elselbl) in 
        let jlbl = TAC_Label(joinlbl) in 
        let jjmp = TAC_Jump(joinlbl) in 
        !currNode.blocks <- !currNode.blocks @ [be];
        let prevNode : cfg_node = !currNode in
        (* true node*)
        currNode := {
          label = tlbl;
          comment = tcomm;
          blocks = [];
          true_branch = None;
          false_branch = None;
          parent_branches = [Some(prevNode)];
        };
        prevNode.true_branch <- Some(!currNode);
        let tinstr, texp = convert astthen.exp_kind (var) cname mname in 
        !currNode.blocks <- !currNode.blocks @ [jjmp];
        (* false node *)
        currNode := {
          label = elbl;
          comment = ecomm;
          blocks = [];
          true_branch = None;
          false_branch = None;
          parent_branches = [Some(prevNode)];
        };
        prevNode.false_branch <- Some(!currNode);
        let einstr, eexp = convert astelse.exp_kind (var) cname mname in 
        !currNode.blocks <- !currNode.blocks @ [jjmp];
        let joinNode : cfg_node = {
          label = jlbl;
          comment = jcomm;
          blocks = [];
          true_branch = None;
          false_branch = None;
          parent_branches = [prevNode.true_branch] @ [prevNode.false_branch];
        }
        in
        (* merge node *)
        merge_node (prevNode.true_branch) (Some(joinNode));
        merge_node (prevNode.false_branch) (Some(joinNode));
        currNode := joinNode;
        pinstr @ [be] @ [tcomm] @ [tlbl] @ tinstr @ [jjmp] @ [ecomm] @ [elbl] @ einstr @ [jjmp] @ [jcomm] @ [jlbl], TAC_Variable(var)
      | While (pred, astbody) ->
        (* while labels *)
        let predlblname = fresh_label cname mname in 
        let predlbl = TAC_Label(predlblname) in
        let bodylblname = fresh_label cname mname in 
        let bodylbl = TAC_Label(bodylblname) in 
        let predcomm = TAC_Comment(sprintf "Predicate for %s" bodylblname) in
        let exitlblname = fresh_label cname mname in 
        let exitlbl = TAC_Label(exitlblname) in
        let jpred = TAC_Jump(predlblname) in 
        !currNode.blocks <- !currNode.blocks @ [jpred];
        let prevNode = !currNode in
        (* entry/predicate setup *)
        let predvar = fresh_var() in 
        let notpredvar = fresh_var () in 
        currNode := {
          label = predlbl;
          comment = predcomm;
          blocks = [];
          true_branch = None;
          false_branch = None;
          parent_branches = [Some(prevNode)];
        };
        
        let pinstr, pexp = convert pred.exp_kind predvar cname mname in 
        (* let notpred = TAC_Assign_BoolNegate (notpredvar, pexp) in *) 
        let bexit = TAC_Branch_True(notpredvar, exitlblname) in 
        !currNode.blocks <- !currNode.blocks @ [bexit];
        let predNode = !currNode in
        (* append predicate to currNode *)
        (* !currNode.blocks <- !currNode.blocks @ [predlbl] @ pinstr @ [notpred] @ [bexit]; *)
        prevNode.true_branch <- Some(predNode);
        (* body node *)
        let bodycomm = TAC_Comment("while-body") in
        currNode := {
          label = bodylbl;
          comment = bodycomm;
          blocks = [];
          true_branch = None;
          false_branch = None;
          parent_branches = [Some(predNode)];
        };
        predNode.false_branch <- Some(!currNode);
        let binstr, bexp = convert astbody.exp_kind var cname mname in 
        (* let prevNode : cfg_node = !currNode in *)
        !currNode.blocks <- !currNode.blocks @ [jpred];
        (* prevNode.false_branch <- Some(!currNode); (* link notpred = false to body node *) *)
        (* exit node *)
        merge_node predNode.false_branch (Some(predNode));
        let exitcomm = TAC_Comment(sprintf "while-exit for %s-%s" predlblname bodylblname) in
        (* let prevNode : cfg_node = !currNode in  *)
        let bodyNode = !currNode in
        let joinNode = {
          label = exitlbl;
          comment = exitcomm;
          blocks = [TAC_Assign_Default(var, "Object")];
          true_branch = None;
          false_branch = None;
          parent_branches = [Some(predNode); Some(bodyNode)];
        } in
        predNode.true_branch <- Some(joinNode);
        bodyNode.false_branch <- Some(joinNode);
        currNode := joinNode;
        [predlbl] @ pinstr @ [bexit] @ binstr @ [jpred] @ [exitlbl] @ [TAC_Assign_Default(var, "Object")], TAC_Variable(var)
      | Case(e0, caseList) ->
        let i, ta = convert e0.exp_kind (fresh_var ())cname mname in
        Hashtbl.add asm_strings ("ERROR: " ^ string_of_int(e0.loc) ^ ": Exception: case on void\\n") ("voidErrString" ^ string_of_int(!voidCounter));
        Hashtbl.add asm_strings ("ERROR: " ^ string_of_int(e0.loc) ^ 
        ": Exception: case without matching branch: "^ 
        ((match e0.static_type with | Some(stype) -> type_to_str_clean stype | None -> "")) ^
        "(...)\\n") ("caseErrString" ^ string_of_int(!caseErrorCounter));
        voidCounter := !voidCounter + 1;
        caseErrorCounter := !caseErrorCounter + 1;
        let branchInstr = ref [] in
        let tempNode = !currNode in
        List.iter ( fun (Case_Elem ((_, bname), _, exp)) ->
          currNode := {
          label = TAC_Label("None");
          comment = TAC_Comment("None");
          blocks = [];
          true_branch = None;
          false_branch = None;
          parent_branches = [];
        };
          Hashtbl.add ident_tac bname (TAC_Variable(fresh_var()));
          let ta, _ = convert exp.exp_kind var cname mname in
          Hashtbl.remove ident_tac bname;
          branchInstr := !branchInstr @ [ta];
          currNode := tempNode;
          !currNode.true_branch <- None;
          !currNode.false_branch <- None;
        ) caseList;
        
        !currNode.blocks <- !currNode.blocks @ i @ [TAC_Case(var, (tac_expr_to_name ta), caseList, !branchInstr)];
        i@ [TAC_Case(var, (tac_expr_to_name ta), caseList, !branchInstr)], TAC_Variable(var)
      | _ -> [], TAC_Variable("None")
  )
  in
  let rec numTemps (a: exp_kind) : int = (
    match a with
    | Identifier(v) -> 0
    | Integer(i) -> 0
    | Bool(i) -> 0
    | String(i) -> 0
    | Plus(a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Minus(a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Times(a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Divide(a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Lt(a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Le(a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Eq(a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Not(a1) -> numTemps a1.exp_kind
    | Negate(a1) -> numTemps a1.exp_kind
    | Isvoid(a1) -> numTemps a1.exp_kind
    | Block(exp) ->
      List.length exp +
      List.fold_left (fun acc e ->
        max acc (numTemps e.exp_kind)
      ) 0 exp
    | Dynamic_Dispatch(caller, (_, mname), args) -> 
      max (numTemps caller.exp_kind) (
      1+List.fold_left (fun acc e ->
        max acc (numTemps e.exp_kind)
      ) 0 args)
    | Self_Dispatch((_,mname), args) -> 
      1+List.fold_left (fun acc e ->
        max acc (numTemps e.exp_kind)
      ) 0 args
    | Static_Dispatch(caller, _, (_, mname), args) -> 
      max (numTemps caller.exp_kind) (
      1+List.fold_left (fun acc e ->
        max acc (numTemps e.exp_kind)
      ) 0 args)
    | New((_, name)) -> 0
    | Let(bindlist, let_body) ->
      List.length bindlist + numTemps let_body.exp_kind
    | Assign((_, name), exp) -> 1 + numTemps exp.exp_kind(* same as let *)
    (* Need to finish rest of tac for objects and conditionals*)
    | If (pred, astthen, astelse) -> 
      let res = max (numTemps pred.exp_kind) (numTemps astthen.exp_kind) in
      max res (numTemps astelse.exp_kind)
    | While (pred, astbody) ->
      max (numTemps pred.exp_kind ) (numTemps astbody.exp_kind)
    | Case (e0, caseList) ->
      1+max (numTemps e0.exp_kind) (
      List.fold_left (fun acc (Case_Elem(_, _, e)) ->
        max acc (numTemps e.exp_kind)
      ) 0 caseList)
    | _ -> 0
  )
  in
  let call_new fout cname = (
    fprintf fout "\t## new %s\n" cname;
    fprintf fout "\tpushq %%rbp\n";
    fprintf fout "\tpushq %%r12\n";
    fprintf fout "\tmovq $%s..new, %%r14\n" cname;
    fprintf fout "\tcall *%%r14\n";
    fprintf fout "\tpopq %%r12\n";
    fprintf fout "\tpopq %%rbp\n";
  )
  in

  (* reset divCounter *)
  let divCounter = (ref 0) in 
  let voidCounter = (ref 0) in
  let caseErrorCounter = (ref 0) in

  (* convert TAC instructions into asm *)
  let rec tac_to_asm fout stackOffset tac_instruction = (
    match tac_instruction with
    | TAC_Assign_Identifier(var, i) ->
      fprintf fout "\n\t## assign identifier %s <- %s\n" var i;
      if not (Hashtbl.mem envtable i) then (
        stackOffset := !stackOffset + 16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset);
        Hashtbl.add envtable var (sprintf "%d(%%rbp)" !stackOffset);
      ) else ( (* move top of stack *)
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable i);
      );
      fprintf fout "\tmovq %%r14, %d(%%rbp)\n" !stackOffset;
      stackOffset := !stackOffset -16;
      fprintf fout "";
    | TAC_Assign_Int(var, i) ->
      fprintf fout "\n\t## %s <- int %s\n" var i;
      call_new fout "Int";
      fprintf fout "\tmovq $%s, 24(%%r13)\n" i;
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
      stackOffset := !stackOffset -16;
    | TAC_Assign_Bool(var, i) ->
      fprintf fout "\n\t## boolean provided\n";
      call_new fout "Bool";
      let bool_int = match i with 
      | "true" -> 1
      | "false" -> 0 
      | _ -> -1
      in
      fprintf fout "\tmovq $%d, 24(%%r13)\n" bool_int;
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
      stackOffset := !stackOffset -16;
    | TAC_Assign_String(var, i) ->
      fprintf fout "\n\t## string provided %s\n" i;
      Hashtbl.add asm_strings i ("string" ^ string_of_int(!stringCounter));
      call_new fout "String";
      fprintf fout "\tmovq $%s, 24(%%r13)\n" ("string" ^ string_of_int(!stringCounter));
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
      stackOffset := !stackOffset - 16;
      stringCounter := !stringCounter + 1;
    | TAC_Assign_Plus(var, i1, i2) ->
      fprintf fout "\n\t## addition\n";
      fprintf fout "\n\t# Arithmetic Add\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i2)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i2));
      );
      fprintf fout "\tmovq 24(%%r14), %%r14\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i1)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r15\n" (Hashtbl.find envtable (tac_expr_to_name i1));
      );
      fprintf fout "\tmovq 24(%%r15), %%r15\n";
      fprintf fout "\taddq %%r14, %%r15\n";
      fprintf fout "\tpushq %%r15\n";
      call_new fout "Int";
      fprintf fout "\tpopq %%r15\n";
      fprintf fout "\tmovq %%r15, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_Minus(var, i1, i2) ->
      fprintf fout "\n\t## subtraction\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i2)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i2));
      );
      fprintf fout "\tmovq 24(%%r14), %%r14\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i1)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r15\n" (Hashtbl.find envtable (tac_expr_to_name i1));
      );
      fprintf fout "\tmovq 24(%%r15), %%r15\n";
      fprintf fout "\tsubq %%r14, %%r15\n";
      fprintf fout "\tpushq %%r15\n";
      call_new fout "Int";
      fprintf fout "\tpopq %%r15\n";
      fprintf fout "\tmovq %%r15, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_Times(var, i1, i2) ->
      fprintf fout "\n\t## multiplication\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i2)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i2));
      );
      fprintf fout "\tmovq 24(%%r14), %%r14\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i1)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r15\n" (Hashtbl.find envtable (tac_expr_to_name i1));
      );
      fprintf fout "\tmovq 24(%%r15), %%r15\n";
      fprintf fout "\timulq %%r14, %%r15\n";
      fprintf fout "\tpushq %%r15\n";
      call_new fout "Int";
      fprintf fout "\tpopq %%r15\n";
      fprintf fout "\tmovq %%r15, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_Div(var, i1, i2) ->
      fprintf fout "\n\t## division\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i2)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i2));
      );
      fprintf fout "\tmovq 24(%%r14), %%r14\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i1)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r15\n" (Hashtbl.find envtable (tac_expr_to_name i1));
      );
      fprintf fout "\tmovl 24(%%r15), %%eax\n";
      fprintf fout "\txorq %%rdx, %%rdx\n";
      fprintf fout "\tcltd\n";

      (* division by zero check *)
      fprintf fout "\tcmpl $0, %%r14d\n";
      fprintf fout "\tjne div_good_%d\n" !divCounter;

      (* use cooloutstr to error out *)
      fprintf fout "\tmovq $%s, %%rdi\n" ("divErrString" ^ string_of_int(!divCounter));
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tcall cooloutstr\n";
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tmovl $1, %%edi\n";
      fprintf fout "\tcall exit\n";

      fprintf fout ".globl div_good_%d\ndiv_good_%d:\n" !divCounter !divCounter;
      fprintf fout "\tidivl %%r14d\n";
      fprintf fout "\tpushq %%rax\n";
      call_new fout "Int";
      fprintf fout "\tpopq %%rax\n";
      fprintf fout "\tmovl %%eax, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
      divCounter := !divCounter + 1;
    | TAC_Assign_Lt(var, i1, i2) ->
      fprintf fout "\n\t## less than\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i2)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i2));
      );
      fprintf fout "\tmovq %%r14, %%rdi\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i1)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r15\n" (Hashtbl.find envtable (tac_expr_to_name i1));
      );
      fprintf fout "\tmovq %%r15, %%rsi\n";
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tcall lt_handler\n";
      fprintf fout "\tpushq %%rax\n";
      call_new fout "Bool";
      fprintf fout "\tpopq %%rax\n";
      fprintf fout "\tmovq %%rax, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_Le(var, i1, i2) ->
      fprintf fout "\n\t## less than or equal to\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i2)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i2));
      );
      fprintf fout "\tmovq %%r14, %%rdi\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i1)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r15\n" (Hashtbl.find envtable (tac_expr_to_name i1));
      );
      fprintf fout "\tmovq %%r15, %%rsi\n";
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tcall le_handler\n";
      fprintf fout "\tpushq %%rax\n";
      call_new fout "Bool";
      fprintf fout "\tpopq %%rax\n";
      fprintf fout "\tmovq %%rax, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_Eq(var, i1, i2) ->
      fprintf fout "\n\t## equality\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i1)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i1));
      );
      fprintf fout "\tmovq %%r14, %%rdi\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i2)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r15\n" (Hashtbl.find envtable (tac_expr_to_name i2));
      );
      fprintf fout "\tmovq %%r15, %%rsi\n";
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tcall eq_handler\n";
      fprintf fout "\tpushq %%rax\n";
      call_new fout "Bool";
      fprintf fout "\tpopq %%rax\n";
      fprintf fout "\tmovq %%rax, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_ArithNegate(var, i) ->
      fprintf fout "\n\t## arithmetic negate\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset);
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i));
      );
      fprintf fout "\tmovl 24(%%r14), %%edi\n";
      fprintf fout "\tnegl %%edi\n";
      fprintf fout "\tpushq %%rdi\n";
      call_new fout "Int";
      fprintf fout "\tpopq %%rdi\n";
      fprintf fout "\tmovl %%edi, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_BoolNegate(var, i) ->
      fprintf fout "\n\t## boolean not\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i)) then (
        stackOffset := !stackOffset +16;
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset);
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i));
      );
      fprintf fout "\tmovq 24(%%r14), %%r14\n";
      fprintf fout "\txorq $1, %%r14\n";
      fprintf fout "\tpushq %%r14\n";
      call_new fout "Bool";
      fprintf fout "\tpopq %%r14\n";
      fprintf fout "\tmovq %%r14, 24(%%r13)\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_NullCheck(var, i) ->
      fprintf fout "\n\t## isvoid\n";
      if not(Hashtbl.mem envtable (tac_expr_to_name i)) then (
        stackOffset := !stackOffset + 16; (* pop top of stack *)
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset);
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i));
      );
      fprintf fout "\tpushq %%r12\n";
      fprintf fout "\tpushq %%rbp\n";
      fprintf fout "\tmovq %%r14, %%rdi\n";
      fprintf fout "\tcall is_void\n";
      fprintf fout "\tpopq %%rbp\n";
      fprintf fout "\tpopq %%r12\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
      stackOffset := !stackOffset -16;
    | TAC_Assign_Static_FunctionCall(var, mname, cname, args_vars) ->
      (* assume caller object is already on top of the stack? or maybe its vars, need to check *)
      (* TODO: Add a VOID check for var, if caller is void then do a runtime error*)
      fprintf fout "\n\t## %s <- call %s(...) - Static Dispatch\n" var mname;
      fprintf fout "\tpushq %%r12\n";
      fprintf fout "\tpushq %%rbp\n";
      stackOffset := !stackOffset + 16; (* caller object is on top of the stack*)
      fprintf fout "\tmovq %d(%%rbp), %%r12\n" (!stackOffset);

      (* void check *)
      fprintf fout "\tcmpq $0, %%r12\n";
      fprintf fout "\tjne void_good_%d\n" !voidCounter;

      (* use cooloutstr to error out *)
      fprintf fout "\tmovq $%s, %%rdi\n" ("voidErrString" ^ string_of_int(!voidCounter));
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tcall cooloutstr\n";
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tmovl $1, %%edi\n";
      fprintf fout "\tcall exit\n";

      fprintf fout ".globl void_good_%d\nvoid_good_%d:\n" !voidCounter !voidCounter;
      voidCounter := !voidCounter + 1;
      List.iteri (fun i var -> 
        let var = (tac_expr_to_name var) in
        if not(Hashtbl.mem envtable var) then (
          stackOffset := !stackOffset + 16;
        fprintf fout "\tpushq %d(%%rbp)\n" (!stackOffset);
        ) else (
          fprintf fout "\tpushq %s\n" (Hashtbl.find envtable var);
        )
      ) args_vars;
      let vtableOffset = (Hashtbl.find vtable (cname, mname)) in 
      fprintf fout "\tpushq %%r12\n";
      fprintf fout "\t## call %s.%s (vt+%d)\n" cname mname vtableOffset;
      fprintf fout "\tmovq $%s..vtable, %%r14\n" cname;
      fprintf fout "\tmovq %d(%%r14), %%r14\n" vtableOffset;
      fprintf fout "\tcall *%%r14\n";
      fprintf fout "\taddq $%d, %%rsp\n" (8 + 8 * List.length args_vars);
      fprintf fout "\tpopq %%rbp\n";
      fprintf fout "\tpopq %%r12\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_Dynamic_FunctionCall(var, mname, cname, args_vars) ->
      (* assume caller object is already on top of the stack? or maybe its vars, need to check *)
      (* TODO: Add a VOID check for var, if caller is void then do a runtime error*)
      fprintf fout "\n\t## %s <- call %s(...) - Dynamic Dispatch\n" var mname;
      fprintf fout "\tpushq %%r12\n";
      fprintf fout "\tpushq %%rbp\n";
      stackOffset := !stackOffset + 16; (* caller object is on top of the stack *)
      fprintf fout "\tmovq %d(%%rbp), %%r12\n" (!stackOffset);

      (* void check *)
      fprintf fout "\tcmpq $0, %%r12\n";
      fprintf fout "\tjne void_good_%d\n" !voidCounter;

      (* use cooloutstr to error out *)
      fprintf fout "\tmovq $%s, %%rdi\n" ("voidErrString" ^ string_of_int(!voidCounter));
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tcall cooloutstr\n";
      fprintf fout "\tandq $-16, %%rsp\n";
      fprintf fout "\tmovl $1, %%edi\n";
      fprintf fout "\tcall exit\n";

      fprintf fout ".globl void_good_%d\nvoid_good_%d:\n" !voidCounter !voidCounter;
      voidCounter := !voidCounter + 1;

      List.iteri (fun i var -> 
        let var = (tac_expr_to_name var) in
        if not(Hashtbl.mem envtable var) then (
          stackOffset := !stackOffset + 16;
        fprintf fout "\tpushq %d(%%rbp)\n" (!stackOffset);
        ) else (
          fprintf fout "\tpushq %s\n" (Hashtbl.find envtable var);
        )
        ) args_vars;
        let vtableOffset = (Hashtbl.find vtable (cname, mname)) in 
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\t## call %s.%s (vt+%d)\n" cname mname vtableOffset;
        fprintf fout "\tmovq 16(%%r12), %%r14\n";
        fprintf fout "\tmovq %d(%%r14), %%r14\n" vtableOffset;
        fprintf fout "\tcall *%%r14\n";
        fprintf fout "\taddq $%d, %%rsp\n" (8 + 8 * List.length args_vars);
        fprintf fout "\tpopq %%rbp\n";
        fprintf fout "\tpopq %%r12\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
        stackOffset := !stackOffset -16;
    | TAC_Assign_Self_FunctionCall(var, mname, cname, args_vars) ->
      fprintf fout "\n\t## %s <- call %s(...) - Self Dispatch\n" var mname;
      fprintf fout "\tpushq %%r12\n";
      fprintf fout "\tpushq %%rbp\n";
      List.iteri (fun i var -> 
        let var = (tac_expr_to_name var) in
        if not(Hashtbl.mem envtable var) then (
          stackOffset := !stackOffset + 16;
        fprintf fout "\tpushq %d(%%rbp)\n" (!stackOffset);
        ) else (
          fprintf fout "\tpushq %s\n" (Hashtbl.find envtable var);
        )
        ) args_vars;
      let vtableOffset = (Hashtbl.find vtable (cname, mname)) in 
      fprintf fout "\tpushq %%r12\n";
      fprintf fout "\t## call %s.%s (vt+%d)\n" cname mname vtableOffset;
      fprintf fout "\tmovq 16(%%r12), %%r14\n";
      fprintf fout "\tmovq %d(%%r14), %%r14\n" vtableOffset;
      fprintf fout "\tcall *%%r14\n";
      fprintf fout "\taddq $%d, %%rsp\n" (8 + 8 * List.length args_vars);
      fprintf fout "\tpopq %%rbp\n";
      fprintf fout "\tpopq %%r12\n";
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" (!stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_New(var, name) ->
      fprintf fout "\n\t## new object\n";
      call_new fout name;
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
      stackOffset := !stackOffset -16;
    | TAC_Assign_Default(var, name) ->
      fprintf fout "\n\t## %s <- default %s\n" var name;
      match name with 
      | "Int" | "Bool" | "String" -> 
        call_new fout name;
      | _ ->
        fprintf fout "\tmovq $0, %%r13\n";
      ;
      fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
      Hashtbl.add envtable var (sprintf "%d(%%rbp)" !stackOffset);
      stackOffset := !stackOffset -16;
    | TAC_Assign_Assign(var, i) ->
      fprintf fout "\n\n## TAC_Assign_Assign\n";
      fprintf fout "\n\t## update identifier %s\n" var;
      if not(Hashtbl.mem envtable (tac_expr_to_name i)) then (
        stackOffset := !stackOffset + 16; (* pop top of stack *)
        fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset);
      ) else (
        fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable (tac_expr_to_name i));
      );
      if Hashtbl.mem envtable var then (
        fprintf fout "\tmovq %%r14, %s\n" (Hashtbl.find envtable var);
      );
    | TAC_Branch_True(cond, label) ->
      fprintf fout "\n\t## conditional jump\n";
      if not(Hashtbl.mem envtable cond) then (
        stackOffset := !stackOffset + 16;
        fprintf fout "\tmovq %d(%%rbp), %%r13\n" !stackOffset;
      ) else (
        fprintf fout "\tmovq %s, %%r13\n" (Hashtbl.find envtable cond);
      );
      fprintf fout "\tcmpq $0, 24(%%r13)\n";
      fprintf fout "\tje %s\n" label;
    | TAC_Comment(comment) ->
      fprintf fout "\n## %s\n" comment;
    | TAC_Jump(label) -> 
      fprintf fout "\tjmp %s\n" label;
    | TAC_Label(label) ->
      fprintf fout ".globl %s\n" label;
      fprintf fout "%s:\n" label
    | TAC_Return(label) ->
      fprintf fout "ret\n"
    | TAC_Remove_Let(var) ->
      fprintf fout "\n\t## Propagating Let return down\n";
      if (!stackOffset <= -32) then (
      fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset +16);
      stackOffset := !stackOffset +16;
      fprintf fout "\tmovq %%r14, %d(%%rbp)\n" (!stackOffset+16);
      );
    | TAC_Case(var, i, caseList, tacList) ->
        if (Hashtbl.mem envtable i) then (
            fprintf fout "\tmovq %s, %%r13\n" (Hashtbl.find envtable i);
        ) else (
            stackOffset := !stackOffset + 16;
            fprintf fout "\tmovq %d(%%rbp), %%r13\n" !stackOffset;
        );
        let branchLabels : (string, string) Hashtbl.t = Hashtbl.create 255 in
        let voidLabel = fresh_label "case" "void" in
        let branchTypes = List.map (fun (Case_Elem (_,(_,stype),_)) -> stype) caseList in
        List.iter (fun x -> Hashtbl.add branchLabels x (fresh_label "case" x) ) branchTypes;
        let errorLabel = fresh_label "case" "error" in
        let endLabel = fresh_label "case" "end" in
        let rec addLabel (tname : label) (prevLabel : label) = (
            let nextLabel =
            if not (List.mem tname branchTypes) then (
                Hashtbl.add branchLabels tname prevLabel;
                prevLabel
            ) else (
                Hashtbl.find branchLabels tname
            ) in
            List.iter ( fun x ->
                addLabel x nextLabel;
            ) (Hashtbl.find_all inheritance tname);
        ) in
        addLabel "Object" errorLabel;
        
        (* do a void check on i *)
        fprintf fout "\tcmpq $0, %%r13\n";
        fprintf fout "\tje %s\n" voidLabel;
        (* check for each class tag*)
        fprintf fout "\tmovq 0(%%r13), %%r13\n";
        Hashtbl.iter ( fun cname ctag ->
            fprintf fout "\tmovq $%d, %%r14\n" ctag;
            fprintf fout "\tcmpq %%r14, %%r13\n";
            fprintf fout "\tje %s\n" (Hashtbl.find branchLabels cname);
        ) class_tags;
        (* void branch *)
        fprintf fout ".globl %s\n%s:\n" voidLabel voidLabel;
        fprintf fout "\tmovq $%s, %%rdi\n" ("voidErrString" ^ string_of_int(!voidCounter));
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tcall cooloutstr\n";
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tmovl $1, %%edi\n";
        fprintf fout "\tcall exit\n";
        voidCounter := !voidCounter + 1;
        (* no matching branches *)
        fprintf fout ".globl %s\n%s:\n" errorLabel errorLabel;
        fprintf fout "\tmovq $%s, %%rdi\n" ("caseErrString" ^ string_of_int(!caseErrorCounter));
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tcall cooloutstr\n";
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tmovl $1, %%edi\n";
        fprintf fout "\tcall exit\n";
        caseErrorCounter := !caseErrorCounter + 1;
        (* output code for each branch, starting from the least type first *)
        List.iter2 ( fun (Case_Elem (_, (_, btype), cexp)) tacs->
            fprintf fout ".globl %s\n%s:\n" (Hashtbl.find branchLabels btype) (Hashtbl.find branchLabels btype);
            let tacOffset = ref !stackOffset in
            List.iter ( fun x ->
              tac_to_asm fout tacOffset x;
            ) tacs;
            fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!tacOffset + 16);
            fprintf fout "\tmovq %%r14, %d(%%rbp)\n" (!stackOffset);
            fprintf fout "\tjmp %s\n" endLabel;
        ) caseList tacList;
        stackOffset := !stackOffset -16;
        fprintf fout ".globl %s\n%s:\n" endLabel endLabel;
        (* no branches are picked, output a runtime error*)
    | _ -> fprintf fout ""
  )
  in
  let visitedNodes = ref [] in
  let rec output_asm fout stackOffset cfgNode = (
    match cfgNode with
    | None -> ();
    | Some(cfgNode) -> 
      if (not(List.mem cfgNode.label !visitedNodes) && 
        (List.for_all (fun x -> match x with Some(x) -> List.mem x.label !visitedNodes; | None -> true) cfgNode.parent_branches)) then (
        visitedNodes :=  cfgNode.label :: !visitedNodes;
        tac_to_asm fout stackOffset cfgNode.comment;
        tac_to_asm fout stackOffset cfgNode.label;
        List.iter ( fun x ->
          tac_to_asm fout stackOffset x;
        ) cfgNode.blocks;
        let trueOffset, falseOffset = ref !stackOffset, ref !stackOffset in
        output_asm fout trueOffset cfgNode.true_branch;
        output_asm fout falseOffset cfgNode.false_branch;
        
        
      )
  )
in
  let asmname = Filename.chop_extension fname ^ ".s" in
  let aout = open_out asmname in 
  let print_calloc fout nmemb msize = (
    fprintf fout "\tmovq $%d, %%rdi\n" nmemb;
    fprintf fout "\tmovq $%d, %%rsi\n" msize;
    fprintf aout "\tandq $-16, %%rsp\n";
    fprintf fout "\tcall calloc\n";
    fprintf fout "\tmovq %%rax, %%r12\n";
  ) in
  let print_asm_string fout sname content = (
    fprintf fout ".globl %s\n" sname;
    fprintf fout "%s:\t" sname;
    fprintf fout "## \"%s\"\n" content;
    String.iter(fun c -> (
      fprintf aout ".byte %3d # %c\n" (Char.code c) c;
    )) content;
    fprintf fout ".byte 0\n\n" (* null terminator *)
  ) in 
  
  Hashtbl.add asm_strings "%d" "percent.d";
  Hashtbl.add asm_strings "%ld" "percent.ld";
  Hashtbl.add asm_strings "" "the.empty.string";
  Hashtbl.add asm_strings "%s%s" "concat.string";
  Hashtbl.add asm_strings "ERROR: 0: Exception: String.substr out of range\\n" "substr.error.string";
  Hashtbl.add asm_strings "abort\\n" "abort.string";

  let class_map, impl_map, parent_map, ast = cltype in (
    List.iter ( fun (child,parent) ->
        Hashtbl.add inheritance parent child;
    ) parent_map;
    (* output vtables from impl_map *)
    List.iteri (fun i (cname, methods) -> (
      fprintf aout ".globl %s..vtable\n%s..vtable:\n" cname cname;
      fprintf aout "\t.quad string%d\n" !stringCounter; (* name *)
      (* add class name as a string under the label stringn *)
      Hashtbl.add asm_strings cname ("string" ^ string_of_int(!stringCounter));
      stringCounter := !stringCounter + 1;
      fprintf aout "\t.quad %s..new\n" cname; (* constructor *)
      List.iteri (fun i (mname, _, defclass, _) -> (
        Hashtbl.add vtable (cname, mname) ((i+2) * 8);
        fprintf aout "\t.quad %s.%s\n" defclass mname;
      )) methods
    )) impl_map;

    (* Hashtbl.iter (fun (cname, mname) offset -> 
      printf "%s.%s = %d\n" cname mname offset;  
    ) vtable; *)

    (* output constructors for objects *)
    List.iteri (fun i (cname, attrs) -> (
      fprintf aout ".globl %s..new\n%s..new:\t\t\t## constructor for %s\n" cname cname cname;
      
      (* create activation record *)
      fprintf aout "\tpushq %%rbp\n"; 
      fprintf aout "\tmovq %%rsp, %%rbp\n"; 
      let ntemps = List.fold_left (fun acc (_, _, aexp) ->
        match aexp with
        | Some(aexp) ->
          max acc (numTemps aexp.exp_kind);
        | None -> max acc 0;
      ) 0 attrs + 1 in (* Adding 1 as assuming the return value is in a temporary*)
      fprintf aout "\t## stack room for temporaries: %d\n" ntemps;
      fprintf aout "\tsubq $%d, %%rsp\n" (ntemps * 16);
      (* allocate for class tag, obj size, vtable, attrs *)
      let attrs_ct = (match cname with 
      | "Bool" | "Int" | "String" -> 1
      | "Object" | "IO" -> 0
      | _ -> List.length(attrs)
      ) in 
      let obj_size = attrs_ct + 3 in
      print_calloc aout obj_size 8;

      fprintf aout "\tmovq $%d, 0(%%r12)\n" i; (* class tag *)
      Hashtbl.add class_tags cname i;
      fprintf aout "\tmovq $%d, 8(%%r12)\n" obj_size; 
      fprintf aout "\tmovq $%s..vtable, 16(%%r12)\n" cname;
      Hashtbl.clear envtable;
      Hashtbl.clear ident_tac;
      Hashtbl.add envtable "self" "%r12";
      (* init attributes -- override for internal methods *)
      (match cname with 
      | "Bool" | "Int" -> 
        fprintf aout "\tmovq $0, 24(%%r12)\n";
      | "String" -> 
        fprintf aout "\tmovq $the.empty.string, 24(%%r12)\n";
      | "Object" | "IO" -> 
        fprintf aout "";
      | _ -> (
        List.iteri (fun i (aname, atype, aexp) -> (
          Hashtbl.add ident_tac aname (TAC_Variable(aname));
          Hashtbl.add attrLocations cname (aname, (24+8*i));
          Hashtbl.add envtable aname (sprintf "%d(%%r12)" (24+8*i));

          (* set up default initialization *)
          fprintf aout "\t## self[%d] = %s: %s\n" (3+i) cname aname;
          match atype with
          | "Bool" | "Int" | "String" ->
            call_new aout atype;
            fprintf aout "\tmovq %%r13, %d(%%r12)\n" (24+8*i);
          | _ ->
            fprintf aout "\tmovq $0, %d(%%r12)\n" (24+8*i);
        )) attrs;

        List.iteri (fun i (aname, atype, aexp) -> (
          match aexp with 
          | Some(aexp) -> (
            (* parse expression *)
            fprintf aout "\t## self[%d] = %s: %s\n" (3+i) cname aname;
            (* Hashtbl.clear envtable; *)
            let node : cfg_node = {
              label = TAC_Internal("");
              comment = TAC_Comment("");
              blocks = [];
              true_branch = None;
              false_branch = None;
              parent_branches = [];
            }
            in
            currNode := node;
            visitedNodes := [];  
            let _, _ = convert aexp.exp_kind (fresh_var()) cname aname in
            let stackOffset = ref 0 in
            output_asm aout stackOffset (Some(node));
            fprintf aout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset+16);
            fprintf aout "\tmovq %%r14, %d(%%r12)\n" (24+8*i);
          )
          | None -> (
            (* do nothing since already default initialized *)
          )
        )) attrs;
      ));
      fprintf aout "\tmovq %%r12, %%r13\n";
      fprintf aout "\tmovq %%rbp, %%rsp\n"; 
      fprintf aout "\tpopq %%rbp\n"; 
      fprintf aout "\tret\n"
    )) class_map;

    (* output methods for non-default classes *)
    fprintf aout "\n## USER METHOD BODIES BEGINS\n\n";

    let user_impl_map = List.filter (
      fun (x, _) -> not (List.mem x ["Object"; "IO"; "Int"; "String"; "Bool"])
    ) impl_map in 
    List.iteri (fun cid (cname, methods) -> (
      let non_inherited_methods = List.filter ( 
        fun (_, _, defname, _) -> cname = defname
      ) methods
      in
      List.iteri (fun mid (mname, formals, _, body) -> (
        fprintf aout "## method definition of %s.%s\n" cname mname;
        fprintf aout ".globl %s.%s\n" cname mname;
        fprintf aout "%s.%s:\n" cname mname;
        fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";
        fprintf aout "\tmovq 16(%%rbp), %%r12\n";

        (* add hashtbl offset entries for each formal, relative to %rbp *)
        Hashtbl.clear envtable;
        Hashtbl.clear ident_tac;
        Hashtbl.add envtable "self" "%r12";
        List.iter( fun (aname, loc) ->
          Hashtbl.add envtable aname (sprintf "%d(%%r12)" loc);
          Hashtbl.add ident_tac aname (TAC_Variable(aname));
        ) (List.rev (Hashtbl.find_all attrLocations cname));
        List.iteri (fun i name -> 
          fprintf aout "\t## fp[%d] = %s  %d(%%rbp)\n" (3+i) name (24+8*i);
          Hashtbl.add envtable name (sprintf "%d(%%rbp)" (24+8*i));  
          Hashtbl.add ident_tac name (TAC_Variable(name));
        ) (formals);
        let ntemps = numTemps body.exp_kind + 1 in (* Adding 1 as assuming the return value is in a temporary*)
        fprintf aout "\t## stack room for temporaries: %d\n" ntemps;
        fprintf aout "\tsubq $%d, %%rsp\n" (ntemps * 16);
        let node : cfg_node = {
          label = TAC_Internal("");
          comment = TAC_Comment("\tstart");
          blocks = [];
          true_branch = None;
          false_branch = None;
          parent_branches = [];
        }
        in
        currNode := node;
        visitedNodes := [];
        (* TODO find the AST for the method and then run it *)      
        let _, _ = convert body.exp_kind (fresh_var()) cname mname in
        let stackOffset = ref 0 in
        output_asm aout stackOffset (Some(node));
        fprintf aout "\tmovq %d(%%rbp), %%r13\n" (!stackOffset + 16);
        fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";
      )) non_inherited_methods;
    )) user_impl_map;

    fprintf aout "\n## USER METHOD BODIES ENDS\n";
    fprintf aout "\n## INTERNAL METHOD BODIES BEGINS\n\n";

    (* output internal method bodies - copied from COOL reference compiler *)
    let internal_impl_map = List.filter (
      fun (x, _) -> List.mem x ["Object"; "IO"; "Int"; "String"; "Bool"]
    ) impl_map in 
    List.iteri (fun cid (cname, methods) -> (
      let non_inherited_methods = List.filter ( 
        fun (_, _, defname, _) -> cname = defname
      ) methods
      in
      List.iteri (fun mid (mname, mformals, mdef, _) -> (
        fprintf aout "## method definition of %s.%s\n" mdef mname;
        fprintf aout ".globl %s.%s\n" cname mname;
        fprintf aout "%s.%s:\n" cname mname;
        fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";

        (* modified from reference compiler *)
        (match cname, mname with 
        | "IO", "in_int" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $16, %%rsp\n";

          call_new aout "Int";
          fprintf aout "\tmovq %%r13, %%r14\n";
          
          (* allocate input buffer of size 4096 *)
          fprintf aout "\n\t## calloc input buffer, store ptr in stack\n";
          fprintf aout "\tmovl $1, %%esi\n";
          fprintf aout "\tmovl $4096, %%edi\n";
          fprintf aout "\tcall calloc\n";
          fprintf aout "\tpushq %%rax\n";

          (* read input with fgets into buffer *)
          fprintf aout "\n\t## read input via fgets\n";
          fprintf aout "\tmovq %%rax, %%rdi\n";
          fprintf aout "\tmovq $4096, %%rsi\n";
          fprintf aout "\tmovq stdin(%%rip), %%rdx\n";

          (* 16B align *)
          fprintf aout "\n\t## guarantee 16-byte alignment before call\n";
          fprintf aout "\tandq $-16, %%rsp\n";
          fprintf aout "\tcall fgets\n";

          (* read int *)
          fprintf aout "\n\t## read int\n";
          fprintf aout "\tpopq %%rdi\n";
          fprintf aout "\tmovl $0, %%eax\n";
          fprintf aout "\tpushq %%rax\n";
          fprintf aout "\tmovq $percent.ld, %%rsi\n";
          fprintf aout "\tmovq %%rsp, %%rdx\n";
          

          (* convert input to long with sscaf %ld *)
          fprintf aout "\n\t## guarantee 16-byte alignment before call\n";
          fprintf aout "\tandq $-16, %%rsp\n";
          fprintf aout "\tcall sscanf\n";
          fprintf aout "\tpopq %%rax\n";

          (* check overflow of 32-bit int boundaries *)
          fprintf aout "\n\t## check overflow\n";
          fprintf aout "\tmovq $0, %%rsi\n";
          fprintf aout "\tcmpq $2147483647, %%rax\n";
          fprintf aout "\tcmovg %%rsi, %%rax\n";
          fprintf aout "\tcmpq $-2147483648, %%rax\n";
          fprintf aout "\tcmovl %%rsi, %%rax\n";

          fprintf aout "\n.in_int_end:\n";
          fprintf aout "\t## store int into Int()\n";
          fprintf aout "\tmovq %%rax, %%r13\n";
          fprintf aout "\tmovq %%r13, 24(%%r14)\n";
          fprintf aout "\tmovq %%r14, %%r13\n";
          
        )
        | "IO", "in_string" -> (
          fprintf aout "\tsubq $16, %%rsp\n";
          call_new aout "String";
          fprintf aout "\tmovq %%r13, %%r14\n";
          fprintf aout "\tandq $-16, %%rsp\n";
          fprintf aout "\tcall coolgetstr\n";
          fprintf aout "\tmovq %%rax, %%r13\n";
          fprintf aout "\tmovq %%r13, 24(%%r14)\n";
          fprintf aout "\tmovq %%r14, %%r13\n";
        )
        | "IO", "out_int" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $8, %%rsp\n";
          fprintf aout "\tmovq 24(%%rbp), %%r14\n";
          fprintf aout "\tmovq 24(%%r14), %%r13\n";
          fprintf aout "\tmovq $percent.ld, %%rdi\n";
          fprintf aout "\tmovl %%r13d, %%eax\n";
          fprintf aout "\tcdqe\n";
          fprintf aout "\tmovq %%rax, %%rsi\n";
          fprintf aout "\tmovl $0, %%eax\n";
          fprintf aout "\tandq $-16, %%rsp\n";
          fprintf aout "\tcall printf\n";
          fprintf aout "\tmov %%r12, %%r13\n";
        )
        | "IO", "out_string" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $16, %%rsp\n";
          fprintf aout "\tmovq 24(%%rbp), %%r14\n";
          fprintf aout "\tmovq 24(%%r14), %%r13\n";
          fprintf aout "\tmovq %%r13, %%rdi\n";
          fprintf aout "\tandq $-16, %%rsp\n";
          fprintf aout "\tcall cooloutstr\n";
          fprintf aout "\tmovq %%r12, %%r13\n";
        )
        | "Object", "abort" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $8, %%rsp\n";
          fprintf aout "\tmovq $abort.string, %%rdi\n";
          fprintf aout "\tcall cooloutstr\n";
          fprintf aout "\tmovl $0, %%edi\n";
          fprintf aout "\tcall exit\n";
        )
        | "Object", "copy" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $16, %%rsp\n";

          fprintf aout "\tmovq 8(%%r12), %%r14\n";
          
          fprintf aout "\tmovq $8, %%rsi\n";
          fprintf aout "\tmovq %%r14, %%rdi\n";
          fprintf aout "\tandq $-16, %%rsp\n";
          fprintf aout "\tcall calloc\n";
          fprintf aout "\tmovq %%rax, %%r13\n";
          fprintf aout "\tpushq %%r13\n";

          (* copy loop *)
          fprintf aout ".globl l1\nl1:\n";
          fprintf aout "\tcmpq $0, %%r14\n";
          fprintf aout "\tje l2\n";
          fprintf aout "\tmovq 0(%%r12), %%r15\n";
          fprintf aout "\tmovq %%r15, 0(%%r13)\n";
          fprintf aout "\tmovq $8, %%r15\n";
          fprintf aout "\taddq %%r15, %%r12\n";
          fprintf aout "\taddq %%r15, %%r13\n";
          fprintf aout "\tmovq $1, %%r15\n";
          fprintf aout "\tsubq %%r15, %%r14\n";
          fprintf aout "\tjmp l1\n";
          fprintf aout ".globl l2\nl2:\n";
          
          (* done with copy loop *)
          fprintf aout "\tpopq %%r13\n";
        )
        | "Object", "type_name" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $8, %%rsp\n";
          fprintf aout "\tpushq %%rbp\n";
          fprintf aout "\tpushq %%r12\n";
          fprintf aout "\tmovq $String..new, %%r14\n";
          fprintf aout "\tcall *%%r14\n";
          fprintf aout "\tpopq %%r12\n";
          fprintf aout "\tpopq %%rbp\n";
          fprintf aout "\tmovq 16(%%r12), %%r14\n";
          fprintf aout "\tmovq 0(%%r14), %%r14\n";
          fprintf aout "\tmovq %%r14, 24(%%r13)\n";
        )
        | "String", "length" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $8, %%rsp\n";
          call_new aout "Int";
          fprintf aout "\tmovq %%r13, %%r14\n";
          fprintf aout "\tmovq 24(%%r12), %%r13\n";
          fprintf aout "\tmovq %%r13, %%rdi\n";
          fprintf aout "\tmovl $0, %%eax\n";
          fprintf aout "\tcall coolstrlen\n";
          fprintf aout "\tmovq %%rax, %%r13\n";
          fprintf aout "\tmovq %%r13, 24(%%r14)\n";
          fprintf aout "\tmovq %%r14, %%r13\n";
        )
        | "String", "concat" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $8, %%rsp\n";
          fprintf aout "\t\n## init new string for return\n";
          call_new aout "String";
          fprintf aout "\tmovq %%r13, %%r15\n";
          fprintf aout "\tmovq 24(%%rbp), %%r14\n";
          fprintf aout "\tmovq 24(%%r14), %%r14\n";
          fprintf aout "\tmovq 24(%%r12), %%r13\n";

          fprintf aout "\t\n## rdi is caller (LHS)\n";
          fprintf aout "\tmovq %%r13, %%rdi\n";
          fprintf aout "\tmovq %%r14, %%rsi\n";
          fprintf aout "\tcall coolstrcat\n";
          fprintf aout "\tmovq %%rax, %%r13\n";
          fprintf aout "\tmovq %%r13, 24(%%r15)\n";
          fprintf aout "\tmovq %%r15, %%r13\n";
        )
        | "String", "substr" -> (
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";
          fprintf aout "\tsubq $8, %%rsp\n";
          fprintf aout "\tpushq %%rbp\n";
          fprintf aout "\tpushq %%r12\n";
          fprintf aout "\tmovq $String..new, %%r14\n";
          fprintf aout "\tcall *%%r14\n";
          fprintf aout "\tpopq %%r12\n";
          fprintf aout "\tpopq %%rbp\n";
          fprintf aout "\tmovq %%r13, %%r15\n";
          fprintf aout "\tmovq 32(%%rbp), %%r14\n";
          fprintf aout "\tmovq 24(%%r14), %%r14\n";
          fprintf aout "\tmovq 24(%%rbp), %%r13\n";
          fprintf aout "\tmovq 24(%%r13), %%r13\n";
          fprintf aout "\tmovq 24(%%r12), %%r12\n";
          fprintf aout "\tmovq %%r12, %%rdi\n";
          fprintf aout "\tmovq %%r13, %%rsi ## start index\n ";
          fprintf aout "\tmovq %%r14, %%rdx ## length\n ";
          fprintf aout "\tcall coolsubstr\n";
          fprintf aout "\tmovq %%rax, %%r13\n";
          fprintf aout "\tcmpq $0, %%r13\n";
          fprintf aout "\tjne l3\n";
          fprintf aout "\tmovq $substr.error.string, %%r13\n";
          fprintf aout "\tmovq %%r13, %%rdi\n";
          fprintf aout "\tcall cooloutstr\n";
          fprintf aout "\tmovl $0, %%edi\n";
          fprintf aout "\tcall exit\n";
          fprintf aout ".globl l3\n";
          fprintf aout "l3:\n";
          fprintf aout "\tmovq %%r13, 24(%%r15)\n";
          fprintf aout "\tmovq %%r15, %%r13\n";
        )
        | _ -> fprintf aout "\n## MISSING INTERNAL METHOD DEF for %s.%s\n\n" cname mname;
        );

        fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";
      )) non_inherited_methods
    )) internal_impl_map;

    (* cooloutstr - copied from ref compiler *)
    fprintf aout "\n.globl cooloutstr\n";
    fprintf aout "cooloutstr:\n";
    fprintf aout "\tpushq	%%rbp\n";
    fprintf aout "\tmovq	%%rsp, %%rbp\n";
    fprintf aout "\tsubq	$32, %%rsp\n";
    fprintf aout "\tmovq	%%rdi, -24(%%rbp)\n";
    fprintf aout "\tmovl	$0, -4(%%rbp)\n";
    fprintf aout "\tjmp	.L2\n";
    fprintf aout ".L5:\n";
    fprintf aout "\tmovl	-4(%%rbp), %%eax\n";
    fprintf aout "\tmovslq	%%eax, %%rdx\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rdx, %%rax\n";
    fprintf aout "\tmovzbl	(%%rax), %%eax\n";
    fprintf aout "\tcmpb	$92, %%al\n";
    fprintf aout "\tjne	.L3\n";
    fprintf aout "\tmovl	-4(%%rbp), %%eax\n";
    fprintf aout "\tcltq\n";
    fprintf aout "\tleaq	1(%%rax), %%rdx\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rdx, %%rax\n";
    fprintf aout "\tmovzbl	(%%rax), %%eax\n";
    fprintf aout "\tcmpb	$110, %%al\n";
    fprintf aout "\tjne	.L3\n";
    fprintf aout "\tmovq	stdout(%%rip), %%rax\n";
    fprintf aout "\tmovq	%%rax, %%rsi\n";
    fprintf aout "\tmovl	$10, %%edi\n";
    fprintf aout "\tcall	fputc\n";
    fprintf aout "\taddl	$2, -4(%%rbp)\n";
    fprintf aout "\tjmp	.L2\n";
    fprintf aout ".L3:\n";
    fprintf aout "\tmovl	-4(%%rbp), %%eax\n";
    fprintf aout "\tmovslq	%%eax, %%rdx\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rdx, %%rax\n";
    fprintf aout "\tmovzbl	(%%rax), %%eax\n";
    fprintf aout "\tcmpb	$92, %%al\n";
    fprintf aout "\tjne	.L4\n";
    fprintf aout "\tmovl	-4(%%rbp), %%eax\n";
    fprintf aout "\tcltq\n";
    fprintf aout "\tleaq	1(%%rax), %%rdx\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rdx, %%rax\n";
    fprintf aout "\tmovzbl	(%%rax), %%eax\n";
    fprintf aout "\tcmpb	$116, %%al\n";
    fprintf aout "\tjne	.L4\n";
    fprintf aout "\tmovq	stdout(%%rip), %%rax\n";
    fprintf aout "\tmovq	%%rax, %%rsi\n";
    fprintf aout "\tmovl	$9, %%edi\n";
    fprintf aout "\tcall	fputc\n";
    fprintf aout "\taddl	$2, -4(%%rbp)\n";
    fprintf aout "\tjmp	.L2\n";
    fprintf aout ".L4:\n";
    fprintf aout "\tmovq	stdout(%%rip), %%rdx\n";
    fprintf aout "\tmovl	-4(%%rbp), %%eax\n";
    fprintf aout "\tmovslq	%%eax, %%rcx\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rcx, %%rax\n";
    fprintf aout "\tmovzbl	(%%rax), %%eax\n";
    fprintf aout "\tmovsbl	%%al, %%eax\n";
    fprintf aout "\tmovq	%%rdx, %%rsi\n";
    fprintf aout "\tmovl	%%eax, %%edi\n";
    fprintf aout "\tcall	fputc\n";
    fprintf aout "\taddl	$1, -4(%%rbp)\n";
    fprintf aout ".L2:\n";
    fprintf aout "\tmovl	-4(%%rbp), %%eax\n";
    fprintf aout "\tmovslq	%%eax, %%rdx\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rdx, %%rax\n";
    fprintf aout "\tmovzbl	(%%rax), %%eax\n";
    fprintf aout "\ttestb	%%al, %%al\n";
    fprintf aout "\tjne	.L5\n";
    fprintf aout "\tmovq	stdout(%%rip), %%rax\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	fflush\n";
    fprintf aout "\tnop\n";
    fprintf aout "\tleave\n";
    fprintf aout "\tret\n";
    fprintf aout ".LFE6:\n";
    fprintf aout "\t.size	cooloutstr, .-cooloutstr\n";


    (* coolstrlen - copied from ref compiler *)
    fprintf aout "\n.globl coolstrlen\n";
    fprintf aout "coolstrlen:\n";
    fprintf aout ".LFB7:\n";
    fprintf aout "\tpushq	%%rbp\n";
    fprintf aout "\tmovq %%rsp, %%rbp\n";
    fprintf aout "\tmovq %%rdi, -24(%%rbp)\n";
    fprintf aout "\tmovl $0, -4(%%rbp)\n";
    fprintf aout "\tjmp .L7\n";
    fprintf aout ".L8:\n";
    fprintf aout "\tmovl -4(%%rbp), %%eax\n";
    fprintf aout "\taddl $1, %%eax\n";
    fprintf aout "\tmovl %%eax, -4(%%rbp)\n";
    fprintf aout ".L7:\n";
    fprintf aout "\tmovl -4(%%rbp), %%eax\n";
    fprintf aout "\tmovl %%eax, %%edx\n";
    fprintf aout "\tmovq -24(%%rbp), %%rax\n";
    fprintf aout "\taddq %%rdx, %%rax\n";
    fprintf aout "\tmovzbl (%%rax), %%eax\n";
    fprintf aout "\ttestb %%al, %%al\n";
    fprintf aout "\tjne .L8\n";
    fprintf aout "\tmovl -4(%%rbp), %%eax\n";
    fprintf aout "\tpopq %%rbp\n";
    fprintf aout "\tret\n";
    fprintf aout ".LFE7:\n";
    

    (* coolstrcat - modified from ref compiler *)
    fprintf aout "\n.globl	coolstrcat\n";
    fprintf aout "coolstrcat:\n";
    fprintf aout "\tpushq	%%rbp\n";
    fprintf aout "\tmovq	%%rsp, %%rbp\n";
    fprintf aout "\tpushq	%%rbx\n";
    fprintf aout "\tsubq	$40, %%rsp\n";
    fprintf aout "\tmovq	%%rdi, -40(%%rbp)\n";
    fprintf aout "\tmovq	%%rsi, -48(%%rbp)\n";
    fprintf aout "\tcmpq	$0, -40(%%rbp)\n";
    fprintf aout "\tjne	.L11\n";
    fprintf aout "\tmovq	-48(%%rbp), %%rax\n";
    fprintf aout "\tjmp	.L12\n";
    fprintf aout ".L11:\n";
    fprintf aout "\tcmpq	$0, -48(%%rbp)\n";
    fprintf aout "\tjne	.L13\n";
    fprintf aout "\tmovq	-40(%%rbp), %%rax\n";
    fprintf aout "\tjmp	.L12\n";
    fprintf aout ".L13:\n";
    fprintf aout "\t## calculate return string length\n";
    fprintf aout "\tmovq	-40(%%rbp), %%rax\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	coolstrlen\n";
    fprintf aout "\tmovl	%%eax, %%ebx\n";
    fprintf aout "\tmovq	-48(%%rbp), %%rax\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	coolstrlen\n";
    fprintf aout "\taddl	%%ebx, %%eax\n";
    fprintf aout "\taddl	$1, %%eax\n";
    fprintf aout "\t\n";
    fprintf aout "\t## store string length at -32\n";
    fprintf aout "\tmovl	%%eax, -32(%%rbp)\n";
    fprintf aout "\tmovl	-32(%%rbp), %%eax\n";
    fprintf aout "\tcltq\n";
    fprintf aout "\t\n";
    fprintf aout "\t## allocate new string\n";
    fprintf aout "\tmovq	$1, %%rsi\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	calloc\n";
    fprintf aout "\tmovq	%%rax, -24(%%rbp) ## store char ptr at -24\n";
    fprintf aout "\t\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rdi\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rsi\n";
    fprintf aout "\tmovq	$concat.string, %%rdx\n";
    fprintf aout "\tmovq	-40(%%rbp), %%rcx\n";
    fprintf aout "\tmovq	-48(%%rbp), %%r8\n";
    fprintf aout "\t\n";
    fprintf aout "\tmovl	$0, %%eax\n";
    fprintf aout "\tandq	$-16, %%rsp\n";
    fprintf aout "\tcall	snprintf\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout ".L12:\n";
    fprintf aout "\tmovq -8(%%rbp), %%rbx\n";
    fprintf aout "\tleave\n";
    fprintf aout "\tret\n";


    (* coolgetstr - copied from ref compiler *)
    fprintf aout ".globl	coolgetstr\n";
    fprintf aout "coolgetstr:\n";
    fprintf aout "\tpushq	%%rbp\n";
    fprintf aout "\tmovq	%%rsp, %%rbp\n";
    fprintf aout "\tsubq	$32, %%rsp\n";
    fprintf aout "\tmovq	%%fs:40, %%rax\n";
    fprintf aout "\tmovq	%%rax, -8(%%rbp)\n";
    fprintf aout "\txorl	%%eax, %%eax\n";
    fprintf aout "\tmovq	$0, -32(%%rbp)\n";
    fprintf aout "\tmovq	$0, -24(%%rbp)\n";
    fprintf aout "\tmovq	stdin(%%rip), %%rdx\n";
    fprintf aout "\tleaq	-24(%%rbp), %%rcx\n";
    fprintf aout "\tleaq	-32(%%rbp), %%rax\n";
    fprintf aout "\tmovq	%%rcx, %%rsi\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	getline\n";
    fprintf aout "\tmovq	%%rax, -16(%%rbp)\n";
    fprintf aout "\tcmpq	$-1, -16(%%rbp)\n";
    fprintf aout "\tje	.L15\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rax\n";
    fprintf aout "\ttestq	%%rax, %%rax\n";
    fprintf aout "\tjne	.L16\n";
    fprintf aout ".L15:\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rax\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	free\n";
    fprintf aout "\tmovl	$1, %%edi\n";
    fprintf aout "\tcall	malloc\n";
    fprintf aout "\tmovq	%%rax, -32(%%rbp)\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rax\n";
    fprintf aout "\tmovb	$0, (%%rax)\n";
    fprintf aout "\tjmp	.L17\n";
    fprintf aout ".L16:\n";
    fprintf aout "\tmovq	-16(%%rbp), %%rdx\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rax\n";
    fprintf aout "\tmovl	$0, %%esi\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	memchr\n";
    fprintf aout "\ttestq	%%rax, %%rax\n";
    fprintf aout "\tje	.L18\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rax\n";
    fprintf aout "\tmovb	$0, (%%rax)\n";
    fprintf aout "\tjmp	.L17\n";
    fprintf aout ".L18:\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rdx\n";
    fprintf aout "\tmovq	-16(%%rbp), %%rax\n";
    fprintf aout "\tsubq	$1, %%rax\n";
    fprintf aout "\taddq	%%rdx, %%rax\n";
    fprintf aout "\tmovzbl	(%%rax), %%eax\n";
    fprintf aout "\tcmpb	$10, %%al\n";
    fprintf aout "\tjne	.L17\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rdx\n";
    fprintf aout "\tsubq	$1, -16(%%rbp)\n";
    fprintf aout "\tmovq	-16(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rdx, %%rax\n";
    fprintf aout "\tmovb	$0, (%%rax)\n";
    fprintf aout ".L17:\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rax\n";
    fprintf aout "\tmovq	-8(%%rbp), %%rdx\n";
    fprintf aout "\tsubq	%%fs:40, %%rdx\n";
    fprintf aout "\tje	.L20\n";
    fprintf aout "\tcall	__stack_chk_fail\n";
    fprintf aout ".L20:\n";
    fprintf aout "\tleave\n";
    fprintf aout "\tret\n";

    (* coolsubstr - copied from ref compiler *)
    fprintf aout ".globl	coolsubstr\n";
    fprintf aout "coolsubstr:\n";
    fprintf aout ".LFB10:\n";
    fprintf aout "\tpushq	%%rbp\n";
    fprintf aout "\tmovq	%%rsp, %%rbp\n";
    fprintf aout "\tsubq	$48, %%rsp\n";
    fprintf aout "\tmovq	%%rdi, -24(%%rbp)\n";
    fprintf aout "\tmovq	%%rsi, -32(%%rbp)\n";
    fprintf aout "\tmovq	%%rdx, -40(%%rbp)\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rax\n";
    fprintf aout "\tmovq	%%rax, %%rdi\n";
    fprintf aout "\tcall	coolstrlen\n";
    fprintf aout "\tmovl	%%eax, -4(%%rbp)\n";
    fprintf aout "\tcmpq	$0, -32(%%rbp)\n";
    fprintf aout "\tjs	.L22\n";
    fprintf aout "\tcmpq	$0, -40(%%rbp)\n";
    fprintf aout "\tjs	.L22\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rdx\n";
    fprintf aout "\tmovq	-40(%%rbp), %%rax\n";
    fprintf aout "\taddq	%%rax, %%rdx\n";
    fprintf aout "\tmovl	-4(%%rbp), %%eax\n";
    fprintf aout "\tcltq\n";
    fprintf aout "\tcmpq	%%rax, %%rdx\n";
    fprintf aout "\tjle	.L23\n";
    fprintf aout ".L22:\n";
    fprintf aout "\tmovl	$0, %%eax\n";
    fprintf aout "\tjmp	.L24\n";
    fprintf aout ".L23:\n";
    fprintf aout "\tmovq	-40(%%rbp), %%rax\n";
    fprintf aout "\tmovq	-32(%%rbp), %%rcx\n";
    fprintf aout "\tmovq	-24(%%rbp), %%rdx\n";
    fprintf aout "\taddq	%%rcx, %%rdx\n";
    fprintf aout "\tmovq	%%rax, %%rsi\n";
    fprintf aout "\tmovq	%%rdx, %%rdi\n";
    fprintf aout "\tcall	strndup\n";
    fprintf aout ".L24:\n";
    fprintf aout "\tleave\n";
    fprintf aout "\tret\n";
    fprintf aout ".LFE10:\n";


    fprintf aout "\n## INTERNAL METHOD BODIES END\n";

    (* print out string constants *)
    Hashtbl.iter (fun cname sname -> (
      print_asm_string aout sname cname
    )) asm_strings;

    (* print out less than handler *)
    fprintf aout "\n## LT_HANDLER\n";
    fprintf aout ".globl lt_handler\nlt_handler:\n";
    fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";

    (* void checks *)
    fprintf aout "\n\t## void checks\n";
    fprintf aout "\tcmpq $0, %%rdi\n";
    fprintf aout "\tje lt_false\n";
    fprintf aout "\tcmpq $0, %%rsi\n";
    fprintf aout "\tje lt_false\n";

    (* load class tags *)
    fprintf aout "\n\t## load class tags\n";
    fprintf aout "\tmovq 0(%%rdi), %%r13\n";
    fprintf aout "\tmovq 0(%%rsi), %%r14\n";

    (* jump to correct compare for class *)
    fprintf aout "\n\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Bool");
    fprintf aout "\tje lt_bool\n";
    fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Int");
    fprintf aout "\tje lt_int\n";
    fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "String");
    fprintf aout "\tje lt_string\n";
    fprintf aout "\tjmp lt_false\n";

    (* boolean comparisons *)
    fprintf aout "\n\t.globl lt_bool\n\tlt_bool:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Bool");
    fprintf aout "\tjne lt_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
    fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
    fprintf aout "\tcmpl %%edi, %%esi\n";
    fprintf aout "\tjl lt_true\n";
    fprintf aout "\tjmp lt_false\n";

    (* int comparisons *)
    fprintf aout "\n\t.globl lt_int\n\tlt_int:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Int"); 
    fprintf aout "\tjne lt_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
    fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
    fprintf aout "\tcmpl %%edi, %%esi\n";
    fprintf aout "\tjl lt_true\n";
    fprintf aout "\tjmp lt_false\n";

    (* string comparison *)
    fprintf aout "\n\t.globl lt_string\n\tlt_string:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "String");
    fprintf aout "\tjne lt_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%r15\n";
    fprintf aout "\tmovq 24(%%rsi), %%rdi\n";
    fprintf aout "\tmovq %%r15, %%rsi\n";
    fprintf aout "\tandq $-16, %%rsp\n";
    fprintf aout "\tcall strcmp\n";
    fprintf aout "\tcmpl $0, %%eax\n";
    fprintf aout "\tjl lt_true\n";
    fprintf aout "\tjmp lt_false\n";

    fprintf aout "\n.globl lt_false\nlt_false:\n";
    fprintf aout "\tmovq $0, %%rax\n";
    fprintf aout "\tjmp lt_handler_end\n";
    fprintf aout "\n.globl lt_true\nlt_true:\n";
    fprintf aout "\tmovq $1, %%rax\n";
    fprintf aout "\tjmp lt_handler_end\n";
    fprintf aout "\n.globl lt_handler_end\nlt_handler_end:\n";
    fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";

    (* print out less than or equal to handler *)
    fprintf aout "\n## LE_HANDLER\n";
    fprintf aout ".globl le_handler\nle_handler:\n";
    fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";

    (* void checks *)
    fprintf aout "\n\t## void checks\n";
    fprintf aout "\tcmpq $0, %%rdi\n";
    fprintf aout "\tje le_false\n";
    fprintf aout "\tcmpq $0, %%rsi\n";
    fprintf aout "\tje le_false\n";

    (* compare pointers *)
    fprintf aout "\n\t## compare pointers\n";
    fprintf aout "\tcmpq %%rdi, %%rsi\n";
    fprintf aout "\tje le_true\n";

    (* load class tags *)
    fprintf aout "\n\t## load class tags\n";
    fprintf aout "\tmovq 0(%%rdi), %%r13\n";
    fprintf aout "\tmovq 0(%%rsi), %%r14\n";

    (* jump to correct compare for class *)
    fprintf aout "\n\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Bool");
    fprintf aout "\tje le_bool\n";
    fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Int");
    fprintf aout "\tje le_int\n";
    fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "String");
    fprintf aout "\tje le_string\n";
    fprintf aout "\tjmp le_false\n";

    (* boolean comparisons *)
    fprintf aout "\n\t.globl le_bool\n\tle_bool:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Bool");
    fprintf aout "\tjne le_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
    fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
    fprintf aout "\tcmpl %%edi, %%esi\n";
    fprintf aout "\tjle le_true\n";
    fprintf aout "\tjmp le_false\n";

    (* int comparisons *)
    fprintf aout "\n\t.globl le_int\n\tle_int:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Int");
    fprintf aout "\tjne le_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
    fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
    fprintf aout "\tcmpl %%edi, %%esi\n";
    fprintf aout "\tjle le_true\n";
    fprintf aout "\tjmp le_false\n";

    (* string comparison *)
    fprintf aout "\n\t.globl le_string\n\tle_string:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "String");
    fprintf aout "\tjne le_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%r15\n";
    fprintf aout "\tmovq 24(%%rsi), %%rdi\n";
    fprintf aout "\tmovq %%r15, %%rsi\n";
    fprintf aout "\tandq $-16, %%rsp\n";
    fprintf aout "\tcall strcmp\n";
    fprintf aout "\tcmp $0, %%eax\n";
    fprintf aout "\tjle le_true\n";
    fprintf aout "\tjmp le_false\n";

    fprintf aout "\n.globl le_false\nle_false:\n";
    fprintf aout "\tmovq $0, %%rax\n";
    fprintf aout "\tjmp le_handler_end\n";
    fprintf aout "\n.globl le_true\nle_true:\n";
    fprintf aout "\tmovq $1, %%rax\n";
    fprintf aout "\n\tjmp le_handler_end\n";
    fprintf aout ".globl le_handler_end\nle_handler_end:\n";
    fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";

    (* print out equal handler *)
    fprintf aout "## EQ_HANDLER\n";
    fprintf aout ".globl eq_handler\neq_handler:\n";
    fprintf aout "\tpushq %%rbp\n";
    fprintf aout "\tmovq %%rsp, %%rbp\n";

    (* compare pointers *)
    fprintf aout "\n\t## compare pointers\n";
    fprintf aout "\tcmpq %%rdi, %%rsi\n";
    fprintf aout "\tje eq_true\n";

    (* check if either is void -> false *)
    (* void equality is covered by pointer check *)
    fprintf aout "\n\t## compare void\n";
    fprintf aout "\tcmpq $0, %%rdi\n";
    fprintf aout "\tje eq_false\n";
    fprintf aout "\tcmpq $0, %%rsi\n";
    fprintf aout "\tje eq_false\n";

    (* load class tags *)
    fprintf aout "\n\t## load class tags\n";
    fprintf aout "\tmovq 0(%%rdi), %%r13\n";
    fprintf aout "\tmovq 0(%%rsi), %%r14\n";

    (* jump to correct compare for class *)
    fprintf aout "\n\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Bool");
    fprintf aout "\tje eq_bool\n";
    fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Int");
    fprintf aout "\tje eq_int\n";
    fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "String");
    fprintf aout "\tje eq_string\n";
    fprintf aout "\tjmp eq_false\n";

    (* boolean comparisons *)
    fprintf aout "\n\t.globl eq_bool\n\teq_bool:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Bool");
    fprintf aout "\tjne eq_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
    fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
    fprintf aout "\tcmpl %%edi, %%esi\n";
    fprintf aout "\tje eq_true\n";
    fprintf aout "\tjmp eq_false\n";

    (* int comparisons *)
    fprintf aout "\n\t.globl eq_int\n\teq_int:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Int");
    fprintf aout "\tjne eq_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
    fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
    fprintf aout "\tcmpl %%edi, %%esi\n";
    fprintf aout "\tje eq_true\n";
    fprintf aout "\tjmp eq_false\n";

    (* string comparison *)
    fprintf aout "\n\t.globl eq_string\n";
    fprintf aout "\teq_string:\n";
    fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "String");
    fprintf aout "\tjne eq_false\n";
    fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
    fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
    fprintf aout "\tandq $-16, %%rsp\n";
    fprintf aout "\tcall strcmp\n";
    fprintf aout "\tcmp $0, %%eax\n";
    fprintf aout "\tje eq_true\n";
    fprintf aout "\tjmp eq_false\n";

    (* return in %rax *)
    fprintf aout "\n\t.globl eq_true\n\teq_true:\n";
    fprintf aout "\tmovq $1, %%rax\n";
    fprintf aout "\tjmp eq_handler_end\n";
    fprintf aout "\n\t.globl eq_false\n\teq_false:\n";
    fprintf aout "\tmovq $0, %%rax\n";
    fprintf aout "\tjmp eq_handler_end\n";
    fprintf aout "\n\t.globl eq_handler_end\n";
    fprintf aout "\teq_handler_end:\n";
    fprintf aout "\tmovq %%rbp, %%rsp\n";
    fprintf aout "\tpopq %%rbp\n";
    fprintf aout "\tret\n";


    (* print out is_void *)
    fprintf aout "\n## is_void\n";
    fprintf aout ".globl is_void\nis_void:\n";
    fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";
    fprintf aout "\tpushq %%rdi\n";
    fprintf aout "\tcall Bool..new\n";
    fprintf aout "\tpopq %%rdi\n";
    fprintf aout "\tcmpq $0, %%rdi\n";
    fprintf aout "\tjne is_void_false\n";
    fprintf aout "\tmovq $1, 24(%%r13)\n";
    fprintf aout ".globl is_void_false\nis_void_false:\n";
    fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";
    (* print out program start *)

    fprintf aout "\n## PROGRAM BEGINS HERE\n";
    fprintf aout ".globl start\n";
    fprintf aout "start:\n";
    fprintf aout ".globl main\n";
    fprintf aout ".type main, @function\n";
    fprintf aout "main:\n";
    fprintf aout "\tmovq $Main..new, %%r14\n";
    fprintf aout "\tpushq %%rbp\n";
    fprintf aout "\tcall *%%r14\n";
    fprintf aout "\tpushq %%rbp\n";
    fprintf aout "\tpushq %%r13\n";
    fprintf aout "\tmovq $Main.main, %%r14\n";
    fprintf aout "\tcall *%%r14\n";
    fprintf aout "\t## guarantee 16-byte alignment before call\n";
    fprintf aout "\tandq $-16, %%rsp\n";
    fprintf aout "\tmovl $0, %%edi\n";
    fprintf aout "\tcall exit\n";
  )
) ;;

main ()

