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
  let read_class_map () = () in 
  
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

