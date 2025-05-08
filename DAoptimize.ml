open Atypes
open Bhelpers
module StringSet = Set.Make (String)

let visitedNodes = ref []

let rec ssa (cfgNode : cfg_node option) =
  match cfgNode with
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

        (* insert merge function for every variable in procedure if multiple parents *)
        (match List.length cfgNode.parent_branches with
        | 0 | 1 -> () (* do nothing, single or no parent *)
        | _ ->
            (* multiple parents *)
            let (vars : tac_instr list) = [] in
            let ssa_instr = vars @ ssa_instr in
            ());

        ssa cfgNode.true_branch;
        ssa cfgNode.false_branch)

let optimize (startNode : cfg_node) =
  ( (* 
  while dce (Some(startNode)) StringSet.empty do (
    visitedNodes := [];
  ) done; *) )
