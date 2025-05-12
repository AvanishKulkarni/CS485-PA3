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

            TAC_Assign_Int(var, i)
           (*   
          | TAC_Assign_Bool (var, i) ->
              
          | TAC_Assign_String (var, i) ->
              
          | TAC_Assign_Plus (var, i1, i2) ->
              
          | TAC_Assign_Minus (var, i1, i2) ->
              
          | TAC_Assign_Times (var, i1, i2) ->
              
          | TAC_Assign_Div (var, i1, i2) ->
              
          | TAC_Assign_Lt (var, i1, i2) ->
              
          | TAC_Assign_Le (var, i1, i2) ->
              
          | TAC_Assign_Eq (var, i1, i2) ->
              
          | TAC_Assign_ArithNegate (var, i) ->
              
          | TAC_Assign_BoolNegate (var, i) ->
              
          | TAC_Assign_NullCheck (var, i) ->
              
          | TAC_Assign_Static_FunctionCall (var, mname, cname, args_vars) ->
              
          | TAC_Assign_Dynamic_FunctionCall (var, mname, cname, args_vars) ->
              
          | TAC_Assign_Self_FunctionCall (var, mname, cname, args_vars) ->
              
          | TAC_Assign_New (var, name) ->
              
          | TAC_Assign_Default (var, name) ->
              
          | TAC_Assign_Assign (var, i) ->
              
          | TAC_Branch_True (cond, label) ->
              
          | TAC_Comment comment -> 
            
          | TAC_Jump label -> 
            
          | TAC_Label label ->
              
          | TAC_Return label -> 
            
          | TAC_Remove_Let var ->
              
          | TAC_Case (var, i, caseList, tacList) ->
              
          | TAC_End_While _ ->
              *)
          | _ ->
            TAC_Comment ("Hello")
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
