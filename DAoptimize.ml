open Atypes
open Bhelpers
open Printf
module StringSet = Set.Make (String)

module SsaSet = Set.Make (struct
  type t = ssa_node

  let compare = Stdlib.compare
end)

let ssaVisitedNodes = ref []
let dceVisitedNodes = ref []
let ssaLabeledNodes : (tac_instr, ssa_node) Hashtbl.t = Hashtbl.create 255
let tacLabeledNodes : (tac_instr, cfg_node) Hashtbl.t = Hashtbl.create 255
let block_num = ref 1
(* let deadCode = ref true *)
(* let blockDead = ref false *)

let rec find_join (node1 : ssa_node option) (node2 : ssa_node option) :
    ssa_node option =
  match (node1, node2) with
  | None, _ -> None
  | _, None -> None
  | Some node1, Some node2 -> (
      if node1.grandparent = node2.grandparent then node1.grandparent
      else
        match find_join node1.grandparent (Some node2) with
        | Some res1 -> Some res1
        | None -> find_join (Some node1) node2.grandparent)

let rec tac_ssa (tacNode : cfg_node option) (ssaNode : ssa_node) : ssa_node =
  match tacNode with
  | None -> ssaNode
  | Some cfgNode ->
      if
        (not (List.mem cfgNode.label !ssaVisitedNodes))
        && not (Hashtbl.mem ssaLabeledNodes cfgNode.label)
      then (
        ssaVisitedNodes := cfgNode.label :: !ssaVisitedNodes;
        (match List.length ssaNode.parent_branches with
        | 0 | 1 -> ()
        | _ -> (
            let join =
              find_join
                (Some (List.hd ssaNode.parent_branches))
                (Some (List.hd (List.tl ssaNode.parent_branches)))
            in
            ssaNode.grandparent <- join;
            match join with
            | Some join ->
                List.iter
                  (fun x ->
                    let var1 =
                      string_of_int
                        (Hashtbl.find
                           (List.hd ssaNode.parent_branches).ssa_names_copy x)
                    in
                    let var2 =
                      string_of_int
                        (Hashtbl.find
                           (List.hd (List.tl ssaNode.parent_branches))
                             .ssa_names_copy x)
                    in
                    if not (var1 = var2 && x = var1) then (
                      Hashtbl.add ssa_names x (Hashtbl.find ssa_names x + 1);
                      Hashtbl.add ssa_reverse
                        (x ^ string_of_int (Hashtbl.find ssa_names x))
                        x;
                      ssaNode.blocks <-
                        ssaNode.blocks
                        @ [
                            SSA_Phi
                              ( x ^ string_of_int (Hashtbl.find ssa_names x),
                                x ^ var1,
                                x ^ var2 );
                          ]))
                  join.defined_vars;
                ssaNode.defined_vars <- ssaNode.defined_vars @ join.defined_vars
            | None -> ()));

        ((let redef_vars = ref [] in
          let ssa_instr = cfgNode.blocks in
          List.iter
            (fun tac ->
              let casePhis = ref [] in
              let new_instr : tac_instr =
                match tac with
                | TAC_Assign_Identifier (var, i) ->
                    let new_i =
                      match Hashtbl.mem ssa_names i with
                      | true -> i ^ string_of_int (Hashtbl.find ssa_names i)
                      | false -> i
                    in
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true ->
                          Hashtbl.add ssa_names var
                            (Hashtbl.find ssa_names var + 1);
                          var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false ->
                          Hashtbl.add ssa_names var 1;
                          var ^ string_of_int 1
                    in
                    redef_vars := !redef_vars @ [ new_var ];
                    Hashtbl.add ssa_reverse new_var var;
                    TAC_Assign_Identifier (new_var, new_i)
                | TAC_Assign_Int (var, i) ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Assign_Int (new_var, i)
                | TAC_Assign_Bool (var, i) ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Assign_Bool (new_var, i)
                | TAC_Assign_String (var, i) ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Assign_String (new_var, i)
                | ( TAC_Assign_Plus (var, i1, i2)
                  | TAC_Assign_Minus (var, i1, i2)
                  | TAC_Assign_Times (var, i1, i2)
                  | TAC_Assign_Div (var, i1, i2)
                  | TAC_Assign_Lt (var, i1, i2)
                  | TAC_Assign_Le (var, i1, i2)
                  | TAC_Assign_Eq (var, i1, i2) ) as x -> (
                    let i1 = tac_expr_to_name i1 in
                    let i2 = tac_expr_to_name i2 in
                    let new_i1 =
                      match Hashtbl.mem ssa_names i1 with
                      | true -> i1 ^ string_of_int (Hashtbl.find ssa_names i1)
                      | false -> i1
                    in
                    let new_i2 =
                      match Hashtbl.mem ssa_names i2 with
                      | true -> i2 ^ string_of_int (Hashtbl.find ssa_names i2)
                      | false -> i2
                    in
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    match x with
                    | TAC_Assign_Plus (_, _, _) ->
                        TAC_Assign_Plus
                          (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                    | TAC_Assign_Minus (_, _, _) ->
                        TAC_Assign_Minus
                          (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                    | TAC_Assign_Times (_, _, _) ->
                        TAC_Assign_Times
                          (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                    | TAC_Assign_Div (_, _, _) ->
                        TAC_Assign_Div
                          (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                    | TAC_Assign_Lt (_, _, _) ->
                        TAC_Assign_Lt
                          (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                    | TAC_Assign_Le (_, _, _) ->
                        TAC_Assign_Le
                          (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                    | TAC_Assign_Eq (_, _, _) ->
                        TAC_Assign_Eq
                          (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                    | _ -> TAC_Comment "")
                | ( TAC_Assign_ArithNegate (var, i)
                  | TAC_Assign_BoolNegate (var, i)
                  | TAC_Assign_NullCheck (var, i) ) as x -> (
                    let i1 = tac_expr_to_name i in
                    let new_i1 =
                      match Hashtbl.mem ssa_names i1 with
                      | true -> i1 ^ string_of_int (Hashtbl.find ssa_names i1)
                      | false -> i1
                    in
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    match x with
                    | TAC_Assign_ArithNegate (_, _) ->
                        TAC_Assign_ArithNegate (new_var, TAC_Variable new_i1)
                    | TAC_Assign_BoolNegate (_, _) ->
                        TAC_Assign_BoolNegate (new_var, TAC_Variable new_i1)
                    | TAC_Assign_NullCheck (_, _) ->
                        TAC_Assign_NullCheck (new_var, TAC_Variable new_i1)
                    | _ -> TAC_Comment "")
                | ( TAC_Assign_Static_FunctionCall (var, mname, cname, args_vars)
                  | TAC_Assign_Dynamic_FunctionCall
                      (var, mname, cname, args_vars)
                  | TAC_Assign_Self_FunctionCall (var, mname, cname, args_vars)
                    ) as x -> (
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    let new_args_vars =
                      List.map
                        (fun (TAC_Variable x) ->
                          match Hashtbl.mem ssa_names x with
                          | true ->
                              TAC_Variable
                                (x ^ string_of_int (Hashtbl.find ssa_names x))
                          | false -> TAC_Variable x)
                        args_vars
                    in
                    match x with
                    | TAC_Assign_Static_FunctionCall _ ->
                        TAC_Assign_Static_FunctionCall
                          (new_var, mname, cname, new_args_vars)
                    | TAC_Assign_Dynamic_FunctionCall _ ->
                        TAC_Assign_Dynamic_FunctionCall
                          (new_var, mname, cname, new_args_vars)
                    | TAC_Assign_Self_FunctionCall _ ->
                        TAC_Assign_Self_FunctionCall
                          (new_var, mname, cname, new_args_vars)
                    | _ -> TAC_Comment "")
                | TAC_Assign_New (var, name) ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Assign_New (new_var, name)
                | TAC_Assign_Default (var, name) ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Assign_Default (new_var, name)
                | TAC_Assign_Assign (var, i) ->
                    let i = tac_expr_to_name i in
                    let new_i =
                      match Hashtbl.mem ssa_names i with
                      | true -> i ^ string_of_int (Hashtbl.find ssa_names i)
                      | false -> i
                    in
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true ->
                          Hashtbl.add ssa_names var
                            (Hashtbl.find ssa_names var + 1);
                          var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false ->
                          Hashtbl.add ssa_names var 1;

                          var ^ string_of_int 1
                    in
                    redef_vars := !redef_vars @ [ new_var ];
                    Hashtbl.add ssa_reverse new_var var;
                    TAC_Assign_Assign (new_var, TAC_Variable new_i)
                | TAC_Branch_True (cond, label) ->
                    let new_var =
                      match Hashtbl.mem ssa_names cond with
                      | true ->
                          cond ^ string_of_int (Hashtbl.find ssa_names cond)
                      | false -> cond
                    in
                    TAC_Branch_True (new_var, label)
                | TAC_Comment comment -> TAC_Comment comment
                | TAC_Jump label -> TAC_Jump label
                | TAC_Label label -> TAC_Label label
                | TAC_Return label -> TAC_Return label
                | TAC_Remove_Let var ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Remove_Let new_var
                | TAC_Case (var, i, caseList, tacList) ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    let new_i =
                      match Hashtbl.mem ssa_names i with
                      | true -> i ^ string_of_int (Hashtbl.find ssa_names i)
                      | false -> i
                    in
                    let newSSAList =
                      List.map2
                        (fun (Case_Elem ((_, var_name), _, _)) node ->
                          let newSSA : ssa_node =
                            {
                              ssa_label = node.label;
                              ssa_comment = node.comment;
                              la_name = string_of_int !block_num;
                              blocks = [];
                              true_branch = None;
                              false_branch = None;
                              parent_branches = [];
                              grandparent = None;
                              defined_vars = [];
                              ssa_names_copy = Hashtbl.create 1;
                            }
                          in
                          block_num := !block_num + 1;
                          tac_ssa (Some node) newSSA)
                        caseList tacList
                    in
                    TAC_SSA_Case (new_var, new_i, caseList, newSSAList)
                | TAC_End_While var ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_End_While new_var
                | _ -> TAC_Comment ""
              in
              ssaNode.blocks <- ssaNode.blocks @ [ new_instr ] @ !casePhis)
            ssa_instr;
          (* insert merge function for every variable in procedure if multiple parents *)
          redef_vars := List.sort_uniq compare !redef_vars;
          ssaNode.defined_vars <- ssaNode.defined_vars @ !redef_vars;
          match List.length cfgNode.parent_branches with
          | 0 -> () (* do nothing, single or no parent *)
          | 1 ->
              ssaNode.defined_vars <-
                (List.hd ssaNode.parent_branches).defined_vars @ !redef_vars;
              ssaNode.defined_vars <-
                List.sort_uniq compare ssaNode.defined_vars
          | _ -> ( (* multiple parents *) ));
         ssaNode.ssa_names_copy <- Hashtbl.copy ssa_names;
         (match cfgNode.false_branch with
         | None -> ()
         | Some node ->
             let ssa_false : ssa_node =
               {
                 ssa_label = node.label;
                 ssa_comment = node.comment;
                 la_name = string_of_int !block_num;
                 blocks = [];
                 true_branch = None;
                 false_branch = None;
                 parent_branches = [ ssaNode ];
                 grandparent = Some ssaNode;
                 defined_vars = [];
                 ssa_names_copy = Hashtbl.create 1;
               }
             in
             block_num := !block_num + 1;
             ssaNode.false_branch <-
               Some (tac_ssa cfgNode.false_branch ssa_false));
         match cfgNode.true_branch with
         | None -> ()
         | Some node ->
             let ssa_true : ssa_node =
               {
                 ssa_label = node.label;
                 ssa_comment = node.comment;
                 la_name = string_of_int !block_num;
                 blocks = [];
                 true_branch = None;
                 false_branch = None;
                 parent_branches = [ ssaNode ];
                 grandparent = Some ssaNode;
                 defined_vars = [];
                 ssa_names_copy = Hashtbl.create 1;
               }
             in
             block_num := !block_num + 1;
             ssaNode.true_branch <- Some (tac_ssa cfgNode.true_branch ssa_true));
        Hashtbl.add ssaLabeledNodes ssaNode.ssa_label ssaNode;
        ssaNode)
      else if Hashtbl.mem ssaLabeledNodes ssaNode.ssa_label then
        Hashtbl.find ssaLabeledNodes ssaNode.ssa_label
      else ssaNode

(*reset visitedNodes before calling ssa_tac*)
let rec ssa_tac (ssaNode : ssa_node option) (tacNode : cfg_node) : cfg_node =
  match ssaNode with
  | None -> tacNode
  | Some cfgNode ->
      if not (Hashtbl.mem tacLabeledNodes tacNode.label) then (
        ssaVisitedNodes := cfgNode.ssa_label :: !ssaVisitedNodes;
        let ssa_instr = cfgNode.blocks in
        List.iter
          (fun tac ->
            let new_instr : tac_instr =
              match tac with
              | TAC_Assign_Identifier (var, i) ->
                  let new_i =
                    match Hashtbl.mem ssa_reverse i with
                    | true -> Hashtbl.find ssa_reverse i
                    | false -> i
                  in
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Assign_Identifier (new_var, new_i)
              | TAC_Assign_Int (var, i) ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Assign_Int (new_var, i)
              | TAC_Assign_Bool (var, i) ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Assign_Bool (new_var, i)
              | TAC_Assign_String (var, i) ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Assign_String (new_var, i)
              | ( TAC_Assign_Plus (var, i1, i2)
                | TAC_Assign_Minus (var, i1, i2)
                | TAC_Assign_Times (var, i1, i2)
                | TAC_Assign_Div (var, i1, i2)
                | TAC_Assign_Lt (var, i1, i2)
                | TAC_Assign_Le (var, i1, i2)
                | TAC_Assign_Eq (var, i1, i2) ) as x -> (
                  let i1 = tac_expr_to_name i1 in
                  let i2 = tac_expr_to_name i2 in
                  let new_i1 =
                    match Hashtbl.mem ssa_reverse i1 with
                    | true -> Hashtbl.find ssa_reverse i1
                    | false -> i1
                  in
                  let new_i2 =
                    match Hashtbl.mem ssa_reverse i2 with
                    | true -> Hashtbl.find ssa_reverse i2
                    | false -> i2
                  in
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  match x with
                  | TAC_Assign_Plus (_, _, _) ->
                      TAC_Assign_Plus
                        (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                  | TAC_Assign_Minus (_, _, _) ->
                      TAC_Assign_Minus
                        (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                  | TAC_Assign_Times (_, _, _) ->
                      TAC_Assign_Times
                        (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                  | TAC_Assign_Div (_, _, _) ->
                      TAC_Assign_Div
                        (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                  | TAC_Assign_Lt (_, _, _) ->
                      TAC_Assign_Lt
                        (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                  | TAC_Assign_Le (_, _, _) ->
                      TAC_Assign_Le
                        (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                  | TAC_Assign_Eq (_, _, _) ->
                      TAC_Assign_Eq
                        (new_var, TAC_Variable new_i1, TAC_Variable new_i2)
                  | _ -> TAC_Comment "")
              | ( TAC_Assign_ArithNegate (var, i)
                | TAC_Assign_BoolNegate (var, i)
                | TAC_Assign_NullCheck (var, i) ) as x -> (
                  let i1 = tac_expr_to_name i in
                  let new_i1 =
                    match Hashtbl.mem ssa_reverse i1 with
                    | true -> Hashtbl.find ssa_reverse i1
                    | false -> i1
                  in
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  match x with
                  | TAC_Assign_ArithNegate (_, _) ->
                      TAC_Assign_ArithNegate (new_var, TAC_Variable new_i1)
                  | TAC_Assign_BoolNegate (_, _) ->
                      TAC_Assign_BoolNegate (new_var, TAC_Variable new_i1)
                  | TAC_Assign_NullCheck (_, _) ->
                      TAC_Assign_NullCheck (new_var, TAC_Variable new_i1)
                  | _ -> TAC_Comment "")
              | ( TAC_Assign_Static_FunctionCall (var, mname, cname, args_vars)
                | TAC_Assign_Dynamic_FunctionCall (var, mname, cname, args_vars)
                | TAC_Assign_Self_FunctionCall (var, mname, cname, args_vars) )
                as x -> (
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  let new_args_vars =
                    List.map
                      (fun (TAC_Variable x) ->
                        match Hashtbl.mem ssa_reverse x with
                        | true -> TAC_Variable (Hashtbl.find ssa_reverse x)
                        | false -> TAC_Variable x)
                      args_vars
                  in
                  match x with
                  | TAC_Assign_Static_FunctionCall _ ->
                      TAC_Assign_Static_FunctionCall
                        (new_var, mname, cname, new_args_vars)
                  | TAC_Assign_Dynamic_FunctionCall _ ->
                      TAC_Assign_Dynamic_FunctionCall
                        (new_var, mname, cname, new_args_vars)
                  | TAC_Assign_Self_FunctionCall _ ->
                      TAC_Assign_Self_FunctionCall
                        (new_var, mname, cname, new_args_vars)
                  | _ -> TAC_Comment "")
              | TAC_Assign_New (var, name) ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Assign_New (new_var, name)
              | TAC_Assign_Default (var, name) ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Assign_Default (new_var, name)
              | TAC_Assign_Assign (var, i) ->
                  let i = tac_expr_to_name i in
                  let new_i =
                    match Hashtbl.mem ssa_reverse i with
                    | true -> Hashtbl.find ssa_reverse i
                    | false -> i
                  in
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Assign_Assign (new_var, TAC_Variable new_i)
              | TAC_Branch_True (cond, label) ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse cond with
                    | true -> Hashtbl.find ssa_reverse cond
                    | false -> cond
                  in
                  TAC_Branch_True (new_var, label)
              | TAC_Comment comment -> TAC_Comment comment
              | TAC_Jump label -> TAC_Jump label
              | TAC_Label label -> TAC_Label label
              | TAC_Return label -> TAC_Return label
              | TAC_Remove_Let var ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_Remove_Let new_var
              | TAC_SSA_Case (var, i, caseList, ssaList) ->
                  let new_i =
                    match Hashtbl.mem ssa_reverse i with
                    | true -> Hashtbl.find ssa_reverse i
                    | false -> i
                  in
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  let tacList =
                    List.map
                      (fun node ->
                        let newTac : cfg_node =
                          {
                            label = node.ssa_label;
                            comment = node.ssa_comment;
                            blocks = [];
                            true_branch = None;
                            false_branch = None;
                            parent_branches = [];
                          }
                        in
                        ssa_tac (Some node) newTac)
                      ssaList
                  in
                  TAC_Case (new_var, new_i, caseList, tacList)
              | TAC_End_While var ->
                  let new_var =
                    match Hashtbl.mem ssa_reverse var with
                    | true -> Hashtbl.find ssa_reverse var
                    | false -> var
                  in
                  TAC_End_While new_var
              | _ -> TAC_Comment ""
            in
            tacNode.blocks <- tacNode.blocks @ [ new_instr ])
          ssa_instr;

        (match cfgNode.false_branch with
        | None -> ()
        | Some node ->
            let tac_false : cfg_node =
              {
                label = node.ssa_label;
                comment = node.ssa_comment;
                blocks = [];
                true_branch = None;
                false_branch = None;
                parent_branches = [ Some tacNode ];
              }
            in

            tacNode.false_branch <-
              Some (ssa_tac cfgNode.false_branch tac_false));
        (match cfgNode.true_branch with
        | None -> ()
        | Some node ->
            let tac_true : cfg_node =
              {
                label = node.ssa_label;
                comment = node.ssa_comment;
                blocks = [];
                true_branch = None;
                false_branch = None;
                parent_branches = [ Some tacNode ];
              }
            in
            tacNode.true_branch <- Some (ssa_tac cfgNode.true_branch tac_true));

        Hashtbl.add tacLabeledNodes tacNode.label tacNode;
        tacNode)
      else if Hashtbl.mem tacLabeledNodes tacNode.label then
        Hashtbl.find tacLabeledNodes tacNode.label
      else tacNode

(* template function to process - create new dceVisitedNodes list above *)
(* let rec dce (node : ssa_node option) =
  match node with
  | None -> ()
  | Some node ->
      if not (List.mem node.label !dceVisitedNodes) then (
        dceVisitedNodes := node.label :: !dceVisitedNodes;
        let dce_instr = node.blocks in

        (match node.true_branch with
        | None -> ()
        | Some true_branch -> dce (Some true_branch));

        match node.false_branch with
        | None -> ()
        | Some false_branch -> dce (Some false_branch)) *)

(* dead code elimination *)(* 
let rec dce (node : ssa_node ref) (in_set : string list) =
  let node = !node in
  if not (List.mem node.ssa_label !dceVisitedNodes) then (
    dceVisitedNodes := node.ssa_label :: !dceVisitedNodes;
    let instr_list = node.blocks in
    (* variables assigned to in this node *)
    let kill_set = node.defined_vars in
    (* variables used in this node *)
    let gen_set =
      List.concat ((List.map (fun i -> get_tac_rhs i)) instr_list)
    in
    (* let gen_set =
          List.map
            (fun x ->
              match Hashtbl.find_opt ssa_reverse x with
              | Some var -> var
              | None -> x)
            rhs
        in *)
    let set_diff lst1 lst2 =
      List.filter (fun x -> not (List.mem x lst2)) lst1
    in
    let set_union lst1 lst2 =
      lst1 @ List.filter (fun x -> not (List.mem x lst1)) lst2
    in
    let out_set =
      List.sort_uniq compare (set_union gen_set (set_diff in_set kill_set))
    in
    let branchUses = (
      match node.true_branch with
      | None -> []
      | Some branch -> checkUsed kill_set branch
    ) @ (match node.false_branch with
    | None -> []
    | Some branch -> checkUsed kill_set branch) in
    let out_set = List.sort_uniq compare (out_set @ branchUses) in
    printf "\n\nBLOCK %s\n" node.la_name;
    printf "in_set: ";
    List.iter (printf "%s ") in_set;
    printf "\n";
    printf "gen_set: ";
    List.iter (printf "%s ") gen_set;
    printf "\n";
    printf "kill_set: ";
    List.iter (printf "%s ") kill_set;
    printf "\n";
    printf "out_set: ";
    List.iter (printf "%s ") out_set;
    printf "\n";
    (* remove dead tac_instr - remove any ssa instructions that 
        do not interact with the out_set, meaning they are irrelevant.
        
        also need to ignore irrelevant tac_instr, like control flow 
        instructions and others *)
    printf "\n\nssa length before dce %d\n" (List.length node.blocks);
  List.iter (fun x -> printf "%s: " (tac_type_to_name x); output_tac_helper stdout x) node.blocks;
    let dce_instr_list =
      List.filter
        (fun instr ->
          let lhs = get_tac_lhs instr in
          let vars = lhs in
          (* return true if anything in vars is in out_set *)
          let live = 
            match instr with 
            | TAC_Assign_Dynamic_FunctionCall _ | TAC_Assign_Static_FunctionCall _ | TAC_Assign_Self_FunctionCall _
            | TAC_Remove_Let _ | TAC_End_While _
            | TAC_Branch_True _ | TAC_Comment _ | TAC_Label _ | TAC_Jump _
            | TAC_Return _ | TAC_Internal _ | TAC_SSA_Case  _
            | TAC_Assign_Assign _ | TAC_Assign_Identifier _ | TAC_Assign_Default _-> true
            | _ ->
            List.exists (fun x -> 
             List.mem x out_set || List.mem x gen_set || x = "t$0"
            ) vars in
          (match live with
          | false ->
            printf "%s : " (tac_type_to_name instr);
            List.iter (fun x -> printf "%s " x) vars;
            printf" %s\n" (string_of_bool live);
          | true -> printf "";);
          live)
        instr_list
    in
    node.blocks <- dce_instr_list;
    printf "\nssa length after dce %d\n" (List.length node.blocks);
    List.iter (fun x -> printf "%s: " (tac_type_to_name x); output_tac_helper stdout x) node.blocks;
    printf "\n";
    (* traverse *)
    (match node.true_branch with
    | None -> ()
    | Some true_branch ->
        let node_true_ref = ref true_branch in
        dce node_true_ref out_set);

    match node.false_branch with
    | None -> ()
    | Some false_branch ->
        let node_false_ref = ref false_branch in
        dce node_false_ref out_set) *)

let rec dce (node : ssa_node ref) =
  let node = !node in
  if not (List.mem node.ssa_label !dceVisitedNodes) then (
    dceVisitedNodes := node.ssa_label :: !dceVisitedNodes;
    let instr_list = node.blocks in
    let gen_set =
      List.concat ((List.map (fun i -> get_tac_rhs i)) instr_list)
    in
    let gen_set = gen_set @ (match (List.find_opt (fun x ->
        List.length (get_tac_lhs x) > 0
      ) (List.rev instr_list)) with
      | Some instr -> get_tac_lhs instr
      | None -> []
      ) in
    printf "\n\ngen_set: ";
    List.iter (printf "%s ") gen_set;
    printf "\n";
    List.iter (fun x -> printf "%s: " (tac_type_to_name x); output_tac_helper stdout x) node.blocks;

    let dce_instr_list =
        List.filter
          (fun instr ->
            let lhs = get_tac_lhs instr in
            let vars = lhs in
            (* return true if anything in vars is in out_set *)
            let live = 
              (* match instr with 
              | TAC_Assign_Dynamic_FunctionCall _ | TAC_Assign_Static_FunctionCall _ | TAC_Assign_Self_FunctionCall _
              | TAC_Remove_Let _ | TAC_End_While _
              | TAC_Branch_True _ | TAC_Comment _ | TAC_Label _ | TAC_Jump _
              | TAC_Return _ | TAC_Internal _ | TAC_SSA_Case  _
              | TAC_Assign_Assign _ | TAC_Assign_Identifier _ | TAC_Assign_Default _-> true
              | _ ->
              List.exists (fun x -> 
                List.mem x gen_set || x = "t$0"
              ) vars in *)
              match instr with
              | TAC_Assign_Int _ | TAC_Assign_String _ | TAC_Assign_Bool _ | TAC_Assign_Plus _
              | TAC_Assign_Minus _ | TAC_Assign_Times _ | TAC_Assign_Div _ | TAC_Assign_Lt _
              | TAC_Assign_Le _ | TAC_Assign_Eq _ | TAC_Assign_BoolNegate _ | TAC_Assign_ArithNegate _
                -> List.exists (fun x -> 
                  List.mem x gen_set || x = "t$0")vars 
              | _ -> true  
              in

            (* (match live with
            | false ->
              printf "%s : " (tac_type_to_name instr);
              List.iter (fun x -> printf "%s " x) vars;
              printf" %s\n" (string_of_bool live);
            | true -> printf "";); *)
            live)
          instr_list
      in
      (* if List.length instr_list != List.length dce_instr_list then(
        blockDead := !blockDead || true;
      ); *)
      node.blocks <- dce_instr_list;
      (match node.true_branch with
        | None -> ()
        | Some true_branch ->
            let node_true_ref = ref true_branch in
            dce node_true_ref);

        (match node.false_branch with
        | None -> ()
        | Some false_branch ->
            let node_false_ref = ref false_branch in
            dce node_false_ref);
  )

let optimize (startNode : cfg_node) : cfg_node =
  Hashtbl.clear ssa_names;
  Hashtbl.clear ssa_reverse;
  Hashtbl.clear ssaLabeledNodes;
  Hashtbl.clear tacLabeledNodes;
  ssaVisitedNodes := [];
  dceVisitedNodes := [];
  block_num := 1;
  let (ssaStart : ssa_node) =
    {
      ssa_label = startNode.label;
      ssa_comment = startNode.comment;
      la_name = string_of_int !block_num;
      blocks = [];
      true_branch = None;
      false_branch = None;
      parent_branches = [];
      grandparent = None;
      defined_vars = [];
      ssa_names_copy = Hashtbl.create 1;
    }
  in
  block_num := !block_num + 1;
  let ssaStart = tac_ssa (Some startNode) ssaStart in

  (* do DCE *)
  let ssaStart = ref ssaStart in
  (* dce ssaStart []; *)
  (* while !deadCode do
    (* blockDead := false; *)
    (* dce ssaStart; *)
    (* deadCode := false; *)
  done; *)
  dce ssaStart;
  dce ssaStart;
  dce ssaStart;
  dce ssaStart;
  dce ssaStart;
  dce ssaStart;

  let (tacStart : cfg_node) =
    {
      label = startNode.label;
      comment = startNode.comment;
      blocks = [];
      true_branch = None;
      false_branch = None;
      parent_branches = [];
    }
  in
  ssaVisitedNodes := [];
  let tacStart = ssa_tac (Some !ssaStart) tacStart in
  tacStart
