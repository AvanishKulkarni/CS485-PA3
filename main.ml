(* Allen Cabrera, Avanish Kulkarni - PA3C2 *)

open Printf

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
  and read_exp () = (

  )
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
  and read_ast () = (

  ) in 
  let rec read_cltype () = 
    let class_map = read_class_map () in
    let impl_map = read_impl_map () in
    let parent_map = read_parent_map () in
    let ast = read_ast () in 
    ()
  in 

  let cltype = read_cltype () in 
  close_in fin;
) ;;

main ()





(* read in cl-type file *)


(* generate cl-tac file for main() in Main *)

