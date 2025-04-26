(* Allen Cabrera, Avanish Kulkarni - PA3 *)

open Printf
open Atypes
open Bhelpers
open Cast
open Dtac
open Easm

let main() = (
  Printexc.record_backtrace true;
  let fname = Sys.argv.(1) in 
  let cltype = read_ast fname () in 
  asm_output fname cltype ()
  
) ;;

main ()

