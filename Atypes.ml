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
and exp = { loc : loc; exp_kind : exp_kind; static_type : static_type option }

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
  | TAC_Assign_ObjectAlloc of
      label * label (* might have to change to tac_expr *)
  | TAC_Assign_ObjectDefault of label * label
  | TAC_Assign_NullCheck of label * tac_expr
  | TAC_Assign_Dynamic_FunctionCall of label * label * label * tac_expr list
  | TAC_Assign_Static_FunctionCall of label * label * label * tac_expr list
  | TAC_Assign_Self_FunctionCall of label * label * label * tac_expr list
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
  | TAC_Case of label * label * case_elem list * cfg_node list
  | TAC_End_While of label
  | TAC_SSA_Merge of label list

and tac_expr = TAC_Variable of label
and label = string
and iconst = string
and bconst = string
and sconst = string

and cfg_node = {
  label : tac_instr;
  comment : tac_instr;
  mutable blocks : tac_instr list;
  mutable true_branch : cfg_node option;
  mutable false_branch : cfg_node option;
  mutable parent_branches : cfg_node option list;
}