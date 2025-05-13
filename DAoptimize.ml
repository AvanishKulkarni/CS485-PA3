open Atypes
open Bhelpers
open Printf
module StringSet = Set.Make (String)

let ssaVisitedNodes = ref []

let ssaLabeledNodes : (tac_instr, ssa_node) Hashtbl.t = Hashtbl.create 255
let tacLabeledNodes : (tac_instr, cfg_node) Hashtbl.t = Hashtbl.create 255

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
  | None ->
      printf "No node found\n";
      ssaNode
  | Some cfgNode ->
      printf "Node found in TAC->SSA\n";
      if
        (not (List.mem cfgNode.label !ssaVisitedNodes))
        (* && (List.for_all
             (fun x ->
               match (x : cfg_node option) with
               | Some x -> List.mem x.label !ssaVisitedNodes
               | None -> true)
             cfgNode.parent_branches) *) && not (Hashtbl.mem ssaLabeledNodes cfgNode.label)
      then (
        printf "Converting from TAC->SSA\n";
        ssaVisitedNodes := cfgNode.label :: !ssaVisitedNodes;
        (match List.length ssaNode.parent_branches with
        | 0 | 1 -> printf "No parent :(\n"
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
              let new_instr : tac_instr =
                match tac with
                | TAC_Assign_Identifier (var, i) ->
                    printf "Converting Identifier to SSA\n";
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
                    redef_vars := !redef_vars @ [ var ];
                    Hashtbl.add ssa_reverse new_var var;
                    TAC_Assign_Identifier (new_var, new_i)
                | TAC_Assign_Int (var, i) ->
                    printf "Converting Int to SSA\n";
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Assign_Int (new_var, i)
                | TAC_Assign_Bool (var, i) ->
                    printf "Converting Bool to SSA\n";
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false -> var
                    in
                    TAC_Assign_Bool (new_var, i)
                | TAC_Assign_String (var, i) ->
                    printf "Converting String to SSA\n";
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
                        TAC_Assign_ArithNegate (new_var, TAC_Variable new_i1)
                    | TAC_Assign_NullCheck (_, _) ->
                        TAC_Assign_ArithNegate (new_var, TAC_Variable new_i1)
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
                    redef_vars := !redef_vars @ [ var ];
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
                    TAC_Comment "Hello" (* DO CASES LATER *)
                | TAC_End_While var ->
                    let new_var =
                      match Hashtbl.mem ssa_names var with
                      | true -> var ^ string_of_int (Hashtbl.find ssa_names var)
                      | false ->
                          Hashtbl.add ssa_names var 1;
                          var
                    in
                    TAC_End_While new_var
                | _ -> TAC_Comment ""
              in
              ssaNode.blocks <- ssaNode.blocks @ [ new_instr ])
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
                 label = node.label;
                 comment = node.comment;
                 blocks = [];
                 true_branch = None;
                 false_branch = None;
                 parent_branches = [ ssaNode ];
                 grandparent = Some ssaNode;
                 defined_vars = [];
                 ssa_names_copy = Hashtbl.create 1;
               }
             in
             ssaNode.false_branch <-
               Some (tac_ssa cfgNode.false_branch ssa_false));
         (match cfgNode.true_branch with
         | None -> ()
         | Some node ->
             let ssa_true : ssa_node =
               {
                 label = node.label;
                 comment = node.comment;
                 blocks = [];
                 true_branch = None;
                 false_branch = None;
                 parent_branches = [ ssaNode ];
                 grandparent = Some ssaNode;
                 defined_vars = [];
                 ssa_names_copy = Hashtbl.create 1;
               }
             in
             ssaNode.true_branch <- Some (tac_ssa cfgNode.true_branch ssa_true)););
        Hashtbl.add ssaLabeledNodes ssaNode.label (ssaNode);
               ssaNode)
      else 
        if (Hashtbl.mem ssaLabeledNodes ssaNode.label) then Hashtbl.find ssaLabeledNodes ssaNode.label
        else
        ssaNode

(*reset visitedNodes before calling ssa_tac*)
let rec ssa_tac (ssaNode : ssa_node option) (tacNode : cfg_node) : cfg_node =
  match ssaNode with
  | None -> tacNode
  | Some cfgNode ->
      printf "Node found in SSA->TAC\n";
      if
        (not (List.mem cfgNode.label !ssaVisitedNodes))
        (* && (List.for_all
             (fun x -> List.mem x.label !ssaVisitedNodes)
             cfgNode.parent_branches) *) && not (Hashtbl.mem tacLabeledNodes tacNode.label)
      then (
        printf "Converting from SSA->TAC\n";
        ssaVisitedNodes := cfgNode.label :: !ssaVisitedNodes;
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
                      TAC_Assign_ArithNegate (new_var, TAC_Variable new_i1)
                  | TAC_Assign_NullCheck (_, _) ->
                      TAC_Assign_ArithNegate (new_var, TAC_Variable new_i1)
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
              | TAC_Case (var, i, caseList, tacList) ->
                  TAC_Comment "Hello" (* DO CASES LATER *)
              | TAC_End_While var ->
                  let new_var =
                    match Hashtbl.mem ssa_names var with
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
                  label = node.label;
                  comment = node.comment;
                  blocks = [TAC_Comment("FALSE WORLD")];
                  true_branch = None;
                  false_branch = None;
                  parent_branches = [ Some tacNode ];
                }
              in
  
              tacNode.false_branch <-
                Some (ssa_tac cfgNode.false_branch tac_false););
        (match cfgNode.true_branch with
        | None -> ()
        | Some node ->
            let tac_true : cfg_node =
              {
                label = node.label;
                comment = node.comment;
                blocks = [TAC_Comment("TRUE WORLD")];
                true_branch = None;
                false_branch = None;
                parent_branches = [ Some tacNode ];
              }
            in
            tacNode.true_branch <- Some (ssa_tac cfgNode.true_branch tac_true));
        
        Hashtbl.add tacLabeledNodes tacNode.label tacNode;
              tacNode)
      else 
        if (Hashtbl.mem tacLabeledNodes tacNode.label) then Hashtbl.find tacLabeledNodes tacNode.label else
        tacNode

let optimize (startNode : cfg_node) : cfg_node =
  (* 
  while dce (Some(startNode)) StringSet.empty do (
    visitedNodes := [];
  ) done; *)
  Hashtbl.clear ssa_names;
  Hashtbl.clear ssa_reverse;
  Hashtbl.clear ssaLabeledNodes;
  Hashtbl.clear tacLabeledNodes;
  ssaVisitedNodes := [];
  printf "Original # of instr: %d\n" (List.length startNode.blocks);
  let (ssaStart : ssa_node) =
    {
      label = startNode.label;
      comment = startNode.comment;
      blocks = [];
      true_branch = None;
      false_branch = None;
      parent_branches = [];
      grandparent = None;
      defined_vars = [];
      ssa_names_copy = Hashtbl.create 1;
    }
  in
  let ssaStart = tac_ssa (Some startNode) ssaStart in
  (match ssaStart.true_branch with
  | None -> printf "Failed to generate a true_branch\n";
  | Some(_) -> printf "Success generating a true branch\n";);
  printf "# of instructions after ssa: %d\n" (List.length ssaStart.blocks);
  printf "TAC->SSA DONE!\n";
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
  let tacStart = ssa_tac (Some ssaStart) tacStart in
  printf "# of instructions after going back to tac: %d\n" (List.length tacStart.blocks);

  printf "SSA->TAC DONE!\n\n";
  tacStart
