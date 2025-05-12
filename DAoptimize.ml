open Atypes
open Bhelpers
module StringSet = Set.Make (String)

let visitedNodes = ref []

let rec ssa (tacNode : cfg_node option) (ssaNode : cfg_node) (defined : label list) =
  match tacNode with
  | None -> ()
  | Some cfgNode ->
      if
        (not (List.mem cfgNode.label !visitedNodes))
        && List.for_all
             (fun x ->
               match x with
               | Some x -> List.mem x.label !visitedNodes
               | None -> true)
             cfgNode.parent_branches
      then (
        visitedNodes := cfgNode.label :: !visitedNodes;

        let ssa_instr = cfgNode.blocks in
        List.iter ( fun tac ->
          let new_instr : tac_instr =
          match tac with
          | TAC_Assign_Identifier (var, i) ->
              let new_i = (match Hashtbl.mem ssa_names i with 
              | true -> i ^ string_of_int (Hashtbl.find ssa_names i)
              | false -> i)
              in
              let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                Hashtbl.add ssa_names var ((Hashtbl.find ssa_names var)+1);
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var ^ (string_of_int 1)
              )
              in
              TAC_Assign_Identifier(new_var, new_i)
          
          | TAC_Assign_Int (var, i) ->
            let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                Hashtbl.add ssa_names var ((Hashtbl.find ssa_names var)+1);
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var ^ (string_of_int 1)
              )
              in
              TAC_Assign_Int(new_var, i)
           
          | TAC_Assign_Bool (var, i) ->
              let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                Hashtbl.add ssa_names var ((Hashtbl.find ssa_names var)+1);
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var ^ (string_of_int 1)
              )
              in
              TAC_Assign_Bool(new_var, i)
          | TAC_Assign_String (var, i) ->
              let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                Hashtbl.add ssa_names var ((Hashtbl.find ssa_names var)+1);
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var ^ (string_of_int 1)
              )
              in
              TAC_Assign_String(new_var, i)
          | TAC_Assign_Plus (var, i1, i2) | TAC_Assign_Minus (var, i1, i2) | TAC_Assign_Times (var, i1, i2)
          | TAC_Assign_Div (var, i1, i2)  | TAC_Assign_Lt (var, i1, i2) | TAC_Assign_Le (var, i1, i2)
          | TAC_Assign_Eq (var, i1, i2) as x-> (
              let i1 = tac_expr_to_name i1 in
              let i2 = tac_expr_to_name i2 in
              let new_i1 = (match Hashtbl.mem ssa_names i1 with 
              | true -> i1 ^ string_of_int (Hashtbl.find ssa_names i1)
              | false -> i1)
              in
              let new_i2 = (match Hashtbl.mem ssa_names i2 with 
              | true -> i2 ^ string_of_int (Hashtbl.find ssa_names i2)
              | false -> i2)
              in
              let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                var
              )
              in
              match x with 
              | TAC_Assign_Plus (_,_,_)->
                TAC_Assign_Plus(new_var, TAC_Variable(new_i1), TAC_Variable(new_i2))
              | TAC_Assign_Minus (_,_,_) ->
                TAC_Assign_Minus(new_var, TAC_Variable(new_i1), TAC_Variable(new_i2))
              | TAC_Assign_Times (_,_,_) ->
                TAC_Assign_Times(new_var, TAC_Variable(new_i1), TAC_Variable(new_i2))
              | TAC_Assign_Div (_,_,_) ->
                TAC_Assign_Div(new_var, TAC_Variable(new_i1), TAC_Variable(new_i2))
              | TAC_Assign_Lt (_,_,_) ->
                TAC_Assign_Lt(new_var, TAC_Variable(new_i1), TAC_Variable(new_i2))
              | TAC_Assign_Le (_,_,_) ->
                TAC_Assign_Le(new_var, TAC_Variable(new_i1), TAC_Variable(new_i2))
              | TAC_Assign_Eq (_,_,_) ->
                TAC_Assign_Eq(new_var, TAC_Variable(new_i1), TAC_Variable(new_i2))
              | _ -> 
                TAC_Comment("")
              
          )
          | TAC_Assign_ArithNegate (var, i) | TAC_Assign_BoolNegate (var, i) | TAC_Assign_NullCheck (var, i) as x-> (
              let i1 = tac_expr_to_name i in
              let new_i1 = (match Hashtbl.mem ssa_names i1 with 
              | true -> i1 ^ string_of_int (Hashtbl.find ssa_names i1)
              | false -> i1)
              in
              let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                var
              )
              in
              match x with 
              | TAC_Assign_ArithNegate (_,_)->
                TAC_Assign_ArithNegate(new_var, TAC_Variable(new_i1))
              | TAC_Assign_BoolNegate (_, _) ->
                TAC_Assign_ArithNegate(new_var, TAC_Variable(new_i1))
              | TAC_Assign_NullCheck (_, _) ->
                TAC_Assign_ArithNegate(new_var, TAC_Variable(new_i1))
              | _ -> 
                TAC_Comment("")
          )
          
          | TAC_Assign_Static_FunctionCall (var, mname, cname, args_vars) | TAC_Assign_Dynamic_FunctionCall (var, mname, cname, args_vars)
          | TAC_Assign_Self_FunctionCall (var, mname, cname, args_vars) as x -> (
            let new_var = (match Hashtbl.mem ssa_names var with 
            | true -> 
              var ^ (string_of_int ((Hashtbl.find ssa_names var)))
            | false -> 
              var
            )
            in
            let new_args_vars = List.map ( fun (TAC_Variable(x)) ->
              match Hashtbl.mem ssa_names x with 
              | true -> 
                TAC_Variable(x ^ (string_of_int ((Hashtbl.find ssa_names x))))
              | false -> 
                TAC_Variable(x)
            ) args_vars
            in
              match x with 
              | TAC_Assign_Static_FunctionCall (_)->
                TAC_Assign_Static_FunctionCall(new_var, mname, cname, new_args_vars)
              | TAC_Assign_Dynamic_FunctionCall (_) ->
                TAC_Assign_Dynamic_FunctionCall(new_var, mname, cname, new_args_vars)
              | TAC_Assign_Self_FunctionCall (_) ->
                TAC_Assign_Self_FunctionCall(new_var, mname, cname, new_args_vars)
              | _ -> 
                TAC_Comment("")
          )
          | TAC_Assign_New (var, name) ->
            let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var
              )
              in
              TAC_Assign_New(new_var, name)
          | TAC_Assign_Default (var, name) ->
            let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                var
              )
              in
              TAC_Assign_Default(new_var, name)
          | TAC_Assign_Assign (var, i) ->
            let i = tac_expr_to_name i in
            let new_i = (match Hashtbl.mem ssa_names i with 
              | true -> i ^ string_of_int (Hashtbl.find ssa_names i)
              | false -> i)
              in
              let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                Hashtbl.add ssa_names var ((Hashtbl.find ssa_names var)+1);
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var ^ (string_of_int 1)
              )
              in
              TAC_Assign_Assign(new_var, TAC_Variable(new_i))
          | TAC_Branch_True (cond, label) ->
            let new_var = (match Hashtbl.mem ssa_names cond with 
              | true -> 
                cond ^ (string_of_int ((Hashtbl.find ssa_names cond)))
              | false -> 
                Hashtbl.add ssa_names cond 1;
                cond
              )
              in
              TAC_Branch_True(new_var, label)
          | TAC_Comment comment -> 
            TAC_Comment (comment)
          | TAC_Jump label -> 
            TAC_Jump label
          | TAC_Label label ->
            TAC_Label label
          | TAC_Return label -> 
            TAC_Return label 
          | TAC_Remove_Let var ->
            let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var
              )
              in
              TAC_Remove_Let(new_var)
          | TAC_Case (var, i, caseList, tacList) ->
            TAC_Comment ("Hello")
          | TAC_End_While (var) ->
            let new_var = (match Hashtbl.mem ssa_names var with 
              | true -> 
                var ^ (string_of_int ((Hashtbl.find ssa_names var)))
              | false -> 
                Hashtbl.add ssa_names var 1;
                var
              )
              in
              TAC_End_While(new_var)
          | _ ->
            TAC_Comment ("")
          in
          ssaNode.blocks <- ssaNode.blocks @ [new_instr]
        ) ssa_instr;
        (* insert merge function for every variable in procedure if multiple parents *)
        (match List.length cfgNode.parent_branches with
        | 0 | 1 -> () (* do nothing, single or no parent *)
        | _ ->
            (* multiple parents *)
            let (vars : tac_instr list) = [] in
            let ssa_instr = vars @ ssa_instr in
            ());

        ssa cfgNode.true_branch ssaNode [];
        ssa cfgNode.false_branch ssaNode [])

let optimize (startNode : cfg_node) =
  ( (* 
  while dce (Some(startNode)) StringSet.empty do (
    visitedNodes := [];
  ) done; *) )
