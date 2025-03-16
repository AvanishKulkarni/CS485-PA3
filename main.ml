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
  | TAC_Assign_FunctionCall of label * label * (tac_expr list) option
  | TAC_Assign_New of label * label
  | TAC_Assign_Default of label * label
  | TAC_Assign_Let of label * tac_expr list
  | TAC_Assign_Assign of label * tac_expr
  | TAC_Branch_True of bconst * label
  | TAC_Comment of string
  | TAC_Label of label
  | TAC_Jump of label
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
let labelCount = ref 1;;

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
  let fresh_label (cname, mname) = (
    let newLabel = 
      (sprintf "%s_%s_%d" cname mname !labelCount) in labelCount := !labelCount + 1;
      newLabel
  ) in
  let ident_tac : (name, (tac_expr)) Hashtbl.t = Hashtbl.create 255 in
  let rec convert (a: exp_kind) (var : name) : (tac_instr list * tac_expr) = (
    match a with
      | Identifier(v) -> 
        let _, name = v in
        if Hashtbl.mem ident_tac name then (
          let ta = Hashtbl.find ident_tac name in
          [TAC_Assign_Identifier(var, (tac_expr_to_name ta))], TAC_Variable(var)
        ) else (
          Hashtbl.add ident_tac name (TAC_Variable(var));
          [TAC_Assign_Identifier(var, name)], TAC_Variable(var)
        );
        
      | Integer(i) ->
        [TAC_Assign_Int(var, string_of_int i)], (TAC_Variable(var))
      | Bool(i) ->
        [TAC_Assign_Bool(var, i)], (TAC_Variable(var))
      | String(i) ->
        [TAC_Assign_String(var, i)], (TAC_Variable(var))
      | Plus(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 in
        let i2, ta2 = convert a2.exp_kind arg2 in
        let to_output = TAC_Assign_Plus(var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Minus(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 in
        let i2, ta2 = convert a2.exp_kind arg2 in
        let to_output = TAC_Assign_Minus(var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Times(a1, a2) -> 
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 in
        let i2, ta2 = convert a2.exp_kind arg2 in
        let to_output = TAC_Assign_Times(var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Divide(a1, a2) -> 
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 in
        let i2, ta2 = convert a2.exp_kind arg2 in
        let to_output = TAC_Assign_Div(var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Lt(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 in
        let i2, ta2 = convert a2.exp_kind arg2 in
        let to_output = TAC_Assign_Lt(var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Le(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 in
        let i2, ta2 = convert a2.exp_kind arg2 in
        let to_output = TAC_Assign_Le(var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Eq(a1, a2) ->
        let arg1 = fresh_var () in
        let arg2 = fresh_var () in
        let i1, ta1 = convert a1.exp_kind arg1 in
        let i2, ta2 = convert a2.exp_kind arg2 in
        let to_output = TAC_Assign_Eq(var, ta1, ta2) in
        (i1 @ i2 @ [to_output]), (TAC_Variable(var))
      | Not(a1) ->
        let i1, ta1 = convert a1.exp_kind (fresh_var()) in
        let to_output = TAC_Assign_BoolNegate(var, ta1) in
        (i1 @ [to_output]), (TAC_Variable(var))
      | Negate(a1) ->
        let i1, ta1 = convert a1.exp_kind (fresh_var())in
        let to_output = TAC_Assign_ArithNegate(var, ta1) in
        (i1 @ [to_output]), (TAC_Variable(var))
      | Isvoid(a1) ->
        let i1, ta1 = convert a1.exp_kind (fresh_var())in
        let to_output = TAC_Assign_NullCheck(var, ta1) in
        (i1 @ [to_output]), (TAC_Variable(var))
      | Block(exp) ->
        let retTacInstr = ref [] in
        let last_statement = List.hd (List.rev exp) in
        let rest_of_list = List.rev(List.tl (List.rev exp)) in
        List.iter( fun e ->
          let i1, ta1 = convert e.exp_kind (fresh_var()) in
          retTacInstr := List.append !retTacInstr i1
        ) rest_of_list;
        let i1, _ = convert last_statement.exp_kind var in
        retTacInstr := List.append !retTacInstr i1;
        (!retTacInstr), (TAC_Variable(var))
      | Dynamic_Dispatch(_, (_, mname), args) ->
        let retTacInstr = ref [] in
        let args_vars = ref [] in
        List.iter(fun a ->
          let i, ta = convert a.exp_kind (fresh_var()) in
          retTacInstr := List.append !retTacInstr i;
          args_vars := List.append !args_vars [ta]
        ) args;
        let to_output = TAC_Assign_FunctionCall(var, mname, Some(!args_vars)) in
        (!retTacInstr @ [to_output]), TAC_Variable(var)
      | Self_Dispatch((_,mname), args) -> 
        let retTacInstr = ref [] in
        let args_vars = ref [] in
        List.iter(fun a ->
          let i, ta = convert a.exp_kind (fresh_var()) in
          retTacInstr := List.append !retTacInstr i;
          args_vars := List.append !args_vars [ta]
        ) args;
        let to_output = TAC_Assign_FunctionCall(var, mname, Some(!args_vars)) in
        (!retTacInstr @ [to_output]), TAC_Variable(var)
      | Static_Dispatch(_, _, (_, mname), args) ->
        let retTacInstr = ref [] in
        let args_vars = ref [] in
        List.iter(fun a ->
          let i, ta = convert a.exp_kind (fresh_var()) in
          retTacInstr := List.append !retTacInstr i;
          args_vars := List.append !args_vars [ta]
        ) args;
        let to_output = TAC_Assign_FunctionCall(var, mname, Some(!args_vars)) in
        (!retTacInstr @ [to_output]), TAC_Variable(var)
      | New((_, name)) ->
        [TAC_Assign_New(var, name)], TAC_Variable(var)
      | Let(bindlist, let_body) ->
        let retTacInstr = ref [] in
        let let_vars = ref [] in
        List.iter
            (fun (Binding ((_, vname), (_, typename), binit)) ->
              match binit with
              (* [Let-Init] *)
              | Some binit ->
                let i, ta = convert binit.exp_kind (fresh_var()) in
                retTacInstr := List.append !retTacInstr i;
                let_vars := List.append !let_vars [ta];
                Hashtbl.add ident_tac vname (ta)
              (* [Let-No-Init] *)
              | None -> 
                let var = fresh_var () in
                retTacInstr := List.append !retTacInstr [TAC_Assign_Default(var, typename)];
                let_vars := List.append !let_vars [TAC_Variable(var)];
                Hashtbl.add ident_tac vname (TAC_Variable(var))
              )
            bindlist;
        let i, ta = convert let_body.exp_kind var in
        List.iter
            (fun (Binding ((_, vname), (_, _), _)) ->
              Hashtbl.remove ident_tac vname;
              )
            bindlist;
        (!retTacInstr @ i), TAC_Variable(var)
      | Assign((_, name), exp) ->
        Hashtbl.add ident_tac name (TAC_Variable(name));
        let i, ta = convert exp.exp_kind name in
        let to_output = TAC_Assign_Assign(var, ta) in
        (i @ [to_output]), (TAC_Variable(name))
      (* Need to finish rest of tac for objects and conditionals*)
      | If (pred, astthen, astelse) -> 
        let thenvar = fresh_var () in 
        let thenlbl = fresh_label ("Main", "main") in 
        let elsevar = fresh_var () in 
        let elselbl = fresh_label ("Main", "main") in 
        let joinlbl = fresh_label ("Main", "main") in 
        let pinstr, pexp = convert pred.exp_kind (thenvar) in 
        let notc = TAC_Assign_BoolNegate(elsevar, pexp) in
        let bt = TAC_Branch_True(thenvar, thenlbl) in
        let be = TAC_Branch_True(elsevar, elselbl) in 
        let tcomm = TAC_Comment("then branch") in 
        let tlbl = TAC_Label(thenlbl) in
        let ecomm = TAC_Comment("else branch") in 
        let elbl = TAC_Label(elselbl) in 
        let jcomm = TAC_Comment("if-join") in 
        let jlbl = TAC_Label(joinlbl) in 
        let jjmp = TAC_Jump(joinlbl) in 
        let tinstr, texp = convert astthen.exp_kind (var) in 
        let einstr, eexp = convert astelse.exp_kind (var) in 

        pinstr @ [notc] @ [be] @ [bt] @ [tcomm] @ [tlbl] @ tinstr @ [jjmp] @ [ecomm] @ [elbl] @ einstr @ [jjmp] @ [jcomm] @ [jlbl], TAC_Variable(var)
      | _ -> [], TAC_Variable("None")
  )
  in
  let output_tac fout tac_instructions = (
    List.iter ( fun x ->
      match x with
      | TAC_Assign_Identifier(var, i) ->
        fprintf fout "%s <- %s\n" var i
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
      | TAC_Assign_FunctionCall(var, mname, None) ->
        fprintf fout "%s <- call %s\n" var mname;
      | TAC_Assign_FunctionCall(var, mname, Some(args_vars)) ->
        fprintf fout "%s <- call %s" var mname;
        List.iter (fun x -> fprintf fout " %s" (tac_expr_to_name x)) args_vars;
        fprintf fout "\n";
      | TAC_Assign_New(var, name) ->
        fprintf fout "%s <- new %s\n" var name
      | TAC_Assign_Default(var, name) ->
        fprintf fout "%s <- default %s\n" var name;
      | TAC_Assign_Assign(var, i) ->
        fprintf fout "%s <- %s\n" var (tac_expr_to_name i);
      | TAC_Branch_True(cond, label) -> 
        fprintf fout "bt %s %s\n" cond label; 
      | TAC_Comment(comment) ->
        fprintf fout "comment %s\n" comment;
      | TAC_Jump(label) -> 
        fprintf fout "jmp %s\n" label;
      | TAC_Label(label) ->
        fprintf fout "label %s\n" label
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
      let tac_instructions, tac_var = convert mexp.exp_kind (fresh_var()) in
      output_tac fout tac_instructions;
      fprintf fout "return %s\n" (tac_expr_to_name tac_var)
    | _ -> fprintf fout ""

  )
) ;;

main ()

