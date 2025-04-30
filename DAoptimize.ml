
open Atypes
open Bhelpers
module StringSet = Set.Make(String)
let visitedNodes = ref []
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
)

let optimize (startNode : cfg_node) = (
  (* 
  while dce (Some(startNode)) StringSet.empty do (
    visitedNodes := [];
  ) done; *)

)