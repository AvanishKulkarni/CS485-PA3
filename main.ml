(* Allen Cabrera, Avanish Kulkarni - PA3C2 *)

open Printf

type static_type =
  | Class of string (* ex "Int" or "Object" *)
  | SELF_TYPE of string

type class_map = int * cool_class list 
and name = string 
and cool_type = string
and cool_class = name * int * attribute list 
and attribute = string * name * cool_type * exp option
and exp = {
  loc : int;
  exp_kind : exp_kind;
  static_type : static_type;
}
and exp_kind =
  | Assign of name * exp (* assign *)
  | Dynamic_Dispatch of exp * name * exp list
  | Static_Dispatch of exp * name * name * exp list
  | Self_Dispatch of name * exp list
  | If of exp * exp * exp
  | While of exp * exp
  | Block of exp list
  | New of name
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
  | Identifier of name
  | Bool of string (* bool *)
  | Let of binding list * exp
  | Case of exp * case_elem list
  | Internal of
      string
      * string
      * string (* return class, class its defined in, method name *)
and binding = Binding of name * cool_type * exp option
and case_elem = Case_Elem of name * cool_type * exp



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
    let _ = read () in (* skip "class_map" *)
    let num_classes = int_of_string(read()) in 
    let classes = read_list read_class in
    (num_classes, classes)
  ) 
  and read_class () = (
    let cname = read () in 
    let nattr = int_of_string(read()) in 
    let attrlist = read_list read_attr in 
    (cname, nattr, attrlist)
  )
  and read_attr () = (
    let init = read () in 
    let aname = read () in 
    let atype = read () in 
    (match init with 
    | "no_initializer" -> ()
    | "initializer" -> read_exp ()
    | _ -> failwith "wtf");
    (init, aname, atype)
  )
in 
  
  let read_impl_map () = () in 

  let read_parent_map () = () in 

  let read_ast () = () in 

  let rec read_cltype () = 
    let class_map = read_class_map () in
    let impl_map = read_impl_map () in
    let parent_map = read_parent_map () in
    let a_ast = read_ast () in 
    ()
  in 

  let cltype = read_cltype () in 
  close_in fin;
) ;;

main ()





(* read in cl-type file *)


(* generate cl-tac file for main() in Main *)

