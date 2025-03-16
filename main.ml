(* Allen Cabrera, Avanish Kulkarni - PA3C2 *)

open Printf

type tac_instr =
  | IConst of iconst
  | BConst of bconst
  | SConst of sconst 
  | Jump of label
  | Label of label
  | Return of string
  | BranchTrue of bconst * label
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
  | TAC_Assign_FunctionCall of label * label * (tac_expr list) option
and arithop = 
  | Add  
  | Sub 
  | Mul 
  | Div 
and compop = 
  | Lt
  | Le
  | Eq
and tac_expr =
  | TAC_Variable of label
and label = string
and iconst = string
and bconst = string 
and sconst = string

let tac_expr_to_name t = match t with TAC_Variable c -> c

type static_type =
  | Class of string (* ex "Int" or "Object" *)
  | SELF_TYPE of string

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
let varCount = ref 0;;

let main() = (
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
  (* TODO: implement reading output expressions from PA2 *)
(*   and read_exp () = (

  ) *)
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
  let rec convert (a: exp_kind) : (tac_instr list * tac_expr) = (
    match a with
      | Identifier(v) -> 
        let _, name = v in
        [], TAC_Variable(name)
      | Integer(i) ->
        let new_var = fresh_var () in
        [TAC_Assign_Int(new_var, string_of_int i)], (TAC_Variable(new_var))
      | Bool(i) ->
        let new_var = fresh_var () in
        [TAC_Assign_Bool(new_var, i)], (TAC_Variable(new_var))
      | String(i) ->
        let new_var = fresh_var () in
        [TAC_Assign_String(new_var, i)], (TAC_Variable(new_var))
      | Plus(a1, a2) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let i2, ta2 = convert a2.exp_kind in
        let to_output = TAC_Assign_Plus(new_var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(new_var))
      | Minus(a1, a2) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let i2, ta2 = convert a2.exp_kind in
        let to_output = TAC_Assign_Minus(new_var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(new_var))
      | Times(a1, a2) -> 
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let i2, ta2 = convert a2.exp_kind in
        let to_output = TAC_Assign_Times(new_var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(new_var))
      | Divide(a1, a2) -> 
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let i2, ta2 = convert a2.exp_kind in
        let to_output = TAC_Assign_Div(new_var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(new_var))
      | Lt(a1, a2) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let i2, ta2 = convert a2.exp_kind in
        let to_output = TAC_Assign_Lt(new_var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(new_var))
      | Le(a1, a2) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let i2, ta2 = convert a2.exp_kind in
        let to_output = TAC_Assign_Le(new_var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(new_var))
      | Eq(a1, a2) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let i2, ta2 = convert a2.exp_kind in
        let to_output = TAC_Assign_Eq(new_var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(new_var))
      | Not(a1) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let to_output = TAC_Assign_BoolNegate(new_var, ta1) in
        (i1 @ [to_output]), (TAC_Variable(new_var))
      | Negate(a1) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let to_output = TAC_Assign_ArithNegate(new_var, ta1) in
        (i1 @ [to_output]), (TAC_Variable(new_var))
      | Isvoid(a1) ->
        let new_var = fresh_var () in
        let i1, ta1 = convert a1.exp_kind in
        let to_output = TAC_Assign_NullCheck(new_var, ta1) in
        (i1 @ [to_output]), (TAC_Variable(new_var))
      (* Need to finish rest of tac for objects and conditionals*)
      | _ -> [], TAC_Variable("None")
  )
  in
  let output_tac fout tac_instructions = (
    List.iter ( fun x ->
      match x with
      | TAC_Assign_Int(var, i) ->
        fprintf fout "%s <- int %s\n" var i
      | TAC_Assign_Bool(var, i) ->
        fprintf fout "%s <- bool %s\n" var i
      | TAC_Assign_String(var, i) ->
        fprintf fout "%s <- string\n%s\n" var i
      | TAC_Assign_Plus(var, i1, i2) ->
        fprintf fout "%s <- + %s %s\n" var (tac_expr_to_name i1) (tac_expr_to_name i2)
      | TAC_Assign_Minus(var, i1, i2) ->
        fprintf fout "%s <- - %s %s\n" var (tac_expr_to_name i1) (tac_expr_to_name i2)
      | TAC_Assign_Times(var, i1, i2) ->
        fprintf fout "%s <- * %s %s\n" var (tac_expr_to_name i1) (tac_expr_to_name i2)
      | TAC_Assign_Div(var, i1, i2) ->
        fprintf fout "%s <- / %s %s\n" var (tac_expr_to_name i1) (tac_expr_to_name i2)
      | TAC_Assign_Lt(var, i1, i2) ->
        fprintf fout "%s <- < %s %s\n" var (tac_expr_to_name i1) (tac_expr_to_name i2)
      | TAC_Assign_Le(var, i1, i2) ->
        fprintf fout "%s <- <= %s %s\n" var (tac_expr_to_name i1) (tac_expr_to_name i2)
      | TAC_Assign_Eq(var, i1, i2) ->
        fprintf fout "%s <- = %s %s\n" var (tac_expr_to_name i1) (tac_expr_to_name i2)
      | TAC_Assign_ArithNegate(var, i) ->
        fprintf fout "%s <- ~ %s\n" var (tac_expr_to_name i)
      | TAC_Assign_BoolNegate(var, i) ->
        fprintf fout "%s <- not %s\n" var (tac_expr_to_name i)
      | TAC_Assign_NullCheck(var, i) ->
        fprintf fout "%s <- isvoid %s\n" var (tac_expr_to_name i)
      (* Need to finish the rest of assign statements for objects and conditional blocks*)
      | _ -> fprintf fout ""

    ) tac_instructions;
  )
  in
  let cltname = Filename.chop_extension fname ^ ".cl-tac" in
  let fout = open_out cltname in
  let _, _, _, ast = cltype in (
    (* given the AST, convert it to a tac instruction *)
    fprintf fout "comment start\n";
    let (_, cname), _, features = List.hd ast in
    let first_method = List.find (fun x -> 
      match x with 
      | Method _ -> true 
      | _ -> false
      ) features in
    match first_method with
    | Method((_, mname), _, _, mexp) ->
      fprintf fout "label %s_%s_0\n" cname mname;
      let tac_instructions, tac_var = convert mexp.exp_kind in
      output_tac fout tac_instructions;
      fprintf fout "return %s\n" (tac_expr_to_name tac_var)
    | _ -> fprintf fout ""

  )
) ;;

main ()





(* read in cl-type file *)


(* generate cl-tac file for main() in Main *)

