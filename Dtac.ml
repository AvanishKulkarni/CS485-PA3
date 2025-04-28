open Printf
open Atypes
open Bhelpers

module StringSet = Set.Make(String)

let tac (a: exp_kind) (var : name) (cname: name) (mname: name) = (
  let rec merge_node curr_node join_node = (
    match curr_node with
    | Some(curr_node) -> (
      match curr_node.true_branch with
      | None -> curr_node.true_branch <- join_node;
      | Some(node_branch) -> merge_node (Some(node_branch)) join_node;
    )
    | None -> ();
  ) in
  let caseBranchType = ref "" in

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
        if !caseBranchType = "" then (
          let to_output = TAC_Assign_Dynamic_FunctionCall(var, mname, callerType, !args_vars) in
          Hashtbl.add asm_strings ("ERROR: " ^ string_of_int(caller.loc) ^ ": Exception: dispatch on void\\n") ("voidErrString" ^ string_of_int(!voidCounter));
          voidCounter := !voidCounter + 1;
          !currNode.blocks <- !currNode.blocks @ [to_output];
          (!retTacInstr @ i @ [to_output]), TAC_Variable(var)
        ) else (
          let to_output = TAC_Assign_Static_FunctionCall(var, mname, !caseBranchType, !args_vars) in
          Hashtbl.add asm_strings ("ERROR: " ^ string_of_int(caller.loc) ^ ": Exception: dispatch on void\\n") ("voidErrString" ^ string_of_int(!voidCounter));
          voidCounter := !voidCounter + 1;
          !currNode.blocks <- !currNode.blocks @ [to_output];
          (!retTacInstr @ i @ [to_output]), TAC_Variable(var)
        )
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
        (* let name =
          match name with
            | "SELF_TYPE" -> cname
            | _ -> name
        in *)
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
        prevNode.true_branch <- Some(!currNode);
        let pinstr, pexp = convert pred.exp_kind predvar cname mname in 
        (* let notpred = TAC_Assign_BoolNegate (notpredvar, pexp) in *) 
        let bexit = TAC_Branch_True(notpredvar, exitlblname) in 
        !currNode.blocks <- !currNode.blocks @ [bexit];
        let predNode = !currNode in
        (* append predicate to currNode *)
        (* !currNode.blocks <- !currNode.blocks @ [predlbl] @ pinstr @ [notpred] @ [bexit]; *)
        
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
        List.iter ( fun (Case_Elem ((_, bname), (_,btype), exp)) ->
          let bvar = fresh_var() in
          let branchNode = {
            label = TAC_Internal("");
            comment = TAC_Internal("");
            blocks = [TAC_Assign_Identifier(bvar, tac_expr_to_name ta)];
            true_branch = None;
            false_branch = None;
            parent_branches = [];
          } in (* currNode is just a placeholder, doesn't get used*)
          currNode := branchNode;
          caseBranchType := btype;
          Hashtbl.add ident_tac bname (TAC_Variable(bvar));
          let _, _ = convert exp.exp_kind var cname mname in
          Hashtbl.remove ident_tac bname;

          (* branchInstr := !branchInstr @ [[TAC_Assign_Identifier(bvar, tac_expr_to_name ta)] @ new_ta]; *)
          branchInstr := !branchInstr @ [branchNode];
          currNode := tempNode;
          !currNode.true_branch <- None;
          !currNode.false_branch <- None;
        ) caseList;
        caseBranchType := "";
        !currNode.blocks <- !currNode.blocks @ i @ [TAC_Case(var, (tac_expr_to_name ta), caseList, !branchInstr)];
        i@ [TAC_Case(var, (tac_expr_to_name ta), caseList, !branchInstr)], TAC_Variable(var)
      | _ -> [], TAC_Variable("None")
  )
  in
  (* make copy of currNode *)
  let currNodeCopy = !currNode in 
  let tlist,texp = convert a var cname mname in

  (* dead code elimination *)
  let visitedNodes = ref [] in
  let rec dce (cfgNode: cfg_node option) (in_set) : bool = (
    match cfgNode with 
    | None -> (false)
    | Some(cfgNode) -> (
      if (not(List.mem cfgNode.label !visitedNodes) && 
        (List.for_all (fun x -> match x with Some(x) -> List.mem x.label !visitedNodes; | None -> true) cfgNode.parent_branches)) then (
          visitedNodes :=  cfgNode.label :: !visitedNodes;
          let gen_set = ref StringSet.empty in 
          let kill_set = ref StringSet.empty in
          
          (* create gen, kill sets *)
          List.iter (fun (i: tac_instr) -> (
            (* add everything that is DEFINED to kill_set *)
            match i with 
            | TAC_Assign_Bool(lbl,_)
            | TAC_Assign_Int(lbl,_)
            | TAC_Assign_String(lbl,_) 
            | TAC_Assign_Assign(lbl,_) 
            | TAC_Assign_New(lbl,_) 
            | TAC_Assign_Default(lbl,_) -> (
              kill_set := StringSet.add lbl !kill_set;
            )
            | TAC_Assign_Identifier(lhs, rhs) -> (
              kill_set := StringSet.add lhs !kill_set;
              gen_set := StringSet.add rhs !gen_set;
            )
            | TAC_Assign_Self_FunctionCall(obj,_,_,args) 
            | TAC_Assign_Static_FunctionCall(obj,_,_,args)
            | TAC_Assign_Dynamic_FunctionCall(obj,_,_,args)
            -> (
              gen_set := StringSet.add obj !gen_set;
              List.iter (fun a -> (
                gen_set := StringSet.add (tac_expr_to_name a) !gen_set
              )) args
            )
            (* add everything is that is USED to gen_set *)
            | TAC_Assign_Plus(lbl,_,_) 
            | TAC_Assign_Minus(lbl,_,_) 
            | TAC_Assign_Times(lbl,_,_)
            | TAC_Assign_Div(lbl,_,_)
            | TAC_Assign_Lt(lbl,_,_)
            | TAC_Assign_Le(lbl,_,_)
            | TAC_Assign_Eq(lbl,_,_)
            | TAC_Assign_BoolNegate(lbl,_)
            | TAC_Assign_ArithNegate(lbl,_)
            | TAC_Assign_NullCheck(lbl,_)
            -> (
              gen_set := StringSet.add lbl !gen_set;
            )
            | _ -> ()
          )) cfgNode.blocks;

          (* delete using "dead" code except assign - things in kill set *)

          (* 
            delete 
          *)

          (* generate new in_set for each branch *)
          let new_in_set = StringSet.union !gen_set (StringSet.diff in_set !kill_set) in 

          (* begin analysis of remaining branches and return logical OR of all *)
          dce cfgNode.true_branch new_in_set || dce cfgNode.false_branch new_in_set || (not (StringSet.is_empty !kill_set));
      ) else (
        false;
      )
    )
  ) in 
  while dce (Some(currNodeCopy)) StringSet.empty do () done;

  (tlist, texp)
)