open Printf
open Atypes
open Bhelpers
open Dtac

let asm_output fname cltype () =
  let rec numTemps (a : exp_kind) : int =
    1
    +
    match a with
    | Identifier v -> 1
    | Integer i -> 1
    | Bool i -> 1
    | String i -> 1
    | Plus (a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Minus (a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Times (a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Divide (a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Lt (a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Le (a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Eq (a1, a2) -> max (numTemps a1.exp_kind) (1 + numTemps a2.exp_kind)
    | Not a1 -> max 1 (numTemps a1.exp_kind)
    | Negate a1 -> max 1 (numTemps a1.exp_kind)
    | Isvoid a1 -> max 1 (numTemps a1.exp_kind)
    | Block exp ->
        List.length exp
        + List.fold_left (fun acc e -> max acc (numTemps e.exp_kind)) 1 exp
    | Dynamic_Dispatch (caller, (_, mname), args) ->
        max (numTemps caller.exp_kind)
          (1
          + List.fold_left (fun acc e -> max acc (numTemps e.exp_kind)) 1 args)
    | Self_Dispatch ((_, mname), args) ->
        1 + List.fold_left (fun acc e -> max acc (numTemps e.exp_kind)) 1 args
    | Static_Dispatch (caller, _, (_, mname), args) ->
        max (numTemps caller.exp_kind)
          (1
          + List.fold_left (fun acc e -> max acc (numTemps e.exp_kind)) 1 args)
    | New (_, name) -> 1
    | Let (bindlist, let_body) ->
        List.length bindlist + numTemps let_body.exp_kind
    | Assign ((_, name), exp) -> 1 + numTemps exp.exp_kind (* same as let *)
    (* Need to finish rest of tac for objects and conditionals*)
    | If (pred, astthen, astelse) ->
        let res = max (numTemps pred.exp_kind) (numTemps astthen.exp_kind) in
        max res (numTemps astelse.exp_kind)
    | While (pred, astbody) ->
        max (numTemps pred.exp_kind) (numTemps astbody.exp_kind)
    | Case (e0, caseList) ->
        1
        + max (numTemps e0.exp_kind)
            (List.fold_left
               (fun acc (Case_Elem (_, _, e)) -> max acc (numTemps e.exp_kind))
               0 caseList)
    | _ -> 0
  in
  let call_new fout cname =
    fprintf fout "\t## new %s\n" cname;
    fprintf fout "\tpushq %%rbp\n";
    fprintf fout "\tpushq %%r12\n";
    fprintf fout "\tmovq $%s..new, %%r14\n" cname;
    fprintf fout "\tcall *%%r14\n";
    fprintf fout "\tpopq %%r12\n";
    fprintf fout "\tpopq %%rbp\n"
  in

  (* reset divCounter *)
  let divCounter = ref 0 in
  let voidCounter = ref 0 in
  let caseErrorCounter = ref 0 in
  let visitedNodes = ref [] in
  (* convert TAC instructions into asm *)
  let rec tac_to_asm fout stackOffset tac_instruction =
    match tac_instruction with
    | TAC_Assign_Identifier (var, i) ->
        fprintf fout "\n\t## assign identifier %s <- %s\n" var i;
        if not (Hashtbl.mem envtable i) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset;
          Hashtbl.add envtable var (sprintf "%d(%%rbp)" !stackOffset))
        else
          (* move top of stack *)
          fprintf fout "\tmovq %s, %%r14\n" (Hashtbl.find envtable i);
        fprintf fout "\tmovq %%r14, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16;
        fprintf fout ""
    | TAC_Assign_Int (var, i) ->
        fprintf fout "\n\t## %s <- int %s\n" var i;
        call_new fout "Int";
        fprintf fout "\tmovq $%s, 24(%%r13)\n" i;
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Bool (var, i) ->
        fprintf fout "\n\t## boolean provided\n";
        call_new fout "Bool";
        let bool_int = match i with "true" -> 1 | "false" -> 0 | _ -> -1 in
        fprintf fout "\tmovq $%d, 24(%%r13)\n" bool_int;
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_String (var, i) ->
        fprintf fout "\n\t## string provided %s\n" i;
        Hashtbl.add asm_strings i ("string" ^ string_of_int !stringCounter);
        call_new fout "String";
        fprintf fout "\tmovq $%s, 24(%%r13)\n"
          ("string" ^ string_of_int !stringCounter);
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16;
        stringCounter := !stringCounter + 1
    | TAC_Assign_Plus (var, i1, i2) ->
        fprintf fout "\n\t## addition\n";
        fprintf fout "\n\t# Arithmetic Add\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i2)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i2));
        fprintf fout "\tmovq 24(%%r14), %%r14\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i1)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r15\n"
            (Hashtbl.find envtable (tac_expr_to_name i1));
        fprintf fout "\tmovq 24(%%r15), %%r15\n";
        fprintf fout "\taddq %%r14, %%r15\n";
        fprintf fout "\tpushq %%r15\n";
        call_new fout "Int";
        fprintf fout "\tpopq %%r15\n";
        fprintf fout "\tmovq %%r15, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Minus (var, i1, i2) ->
        fprintf fout "\n\t## subtraction\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i2)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i2));
        fprintf fout "\tmovq 24(%%r14), %%r14\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i1)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r15\n"
            (Hashtbl.find envtable (tac_expr_to_name i1));
        fprintf fout "\tmovq 24(%%r15), %%r15\n";
        fprintf fout "\tsubq %%r14, %%r15\n";
        fprintf fout "\tpushq %%r15\n";
        call_new fout "Int";
        fprintf fout "\tpopq %%r15\n";
        fprintf fout "\tmovq %%r15, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Times (var, i1, i2) ->
        fprintf fout "\n\t## multiplication\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i2)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i2));
        fprintf fout "\tmovq 24(%%r14), %%r14\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i1)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r15\n"
            (Hashtbl.find envtable (tac_expr_to_name i1));
        fprintf fout "\tmovq 24(%%r15), %%r15\n";
        fprintf fout "\timulq %%r14, %%r15\n";
        fprintf fout "\tpushq %%r15\n";
        call_new fout "Int";
        fprintf fout "\tpopq %%r15\n";
        fprintf fout "\tmovq %%r15, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Div (var, i1, i2) ->
        fprintf fout "\n\t## division\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i2)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i2));
        fprintf fout "\tmovq 24(%%r14), %%r14\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i1)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r15\n"
            (Hashtbl.find envtable (tac_expr_to_name i1));
        fprintf fout "\tmovl 24(%%r15), %%eax\n";
        fprintf fout "\txorq %%rdx, %%rdx\n";
        fprintf fout "\tcltd\n";

        (* division by zero check *)
        fprintf fout "\tcmpl $0, %%r14d\n";
        fprintf fout "\tjne div_good_%d\n" !divCounter;

        (* use cooloutstr to error out *)
        fprintf fout "\tmovq $%s, %%rdi\n"
          ("divErrString" ^ string_of_int !divCounter);
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tcall cooloutstr\n";
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tmovl $1, %%edi\n";
        fprintf fout "\tcall exit\n";

        fprintf fout ".globl div_good_%d\ndiv_good_%d:\n" !divCounter
          !divCounter;
        fprintf fout "\tidivl %%r14d\n";
        fprintf fout "\tpushq %%rax\n";
        call_new fout "Int";
        fprintf fout "\tpopq %%rax\n";
        fprintf fout "\tmovl %%eax, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16;
        divCounter := !divCounter + 1
    | TAC_Assign_Lt (var, i1, i2) ->
        fprintf fout "\n\t## less than\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i2)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i2));
        fprintf fout "\tmovq %%r14, %%rdi\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i1)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r15\n"
            (Hashtbl.find envtable (tac_expr_to_name i1));
        fprintf fout "\tmovq %%r15, %%rsi\n";
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tcall lt_handler\n";
        fprintf fout "\tpushq %%rax\n\tpushq %%rbp\n";
        call_new fout "Bool";
        fprintf fout "\taddq $8, %%rsp\n\tpopq %%rax\n";
        fprintf fout "\tmovq %%rax, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Le (var, i1, i2) ->
        fprintf fout "\n\t## less than or equal to\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i2)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i2));
        fprintf fout "\tmovq %%r14, %%rdi\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i1)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r15\n"
            (Hashtbl.find envtable (tac_expr_to_name i1));
        fprintf fout "\tmovq %%r15, %%rsi\n";
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tcall le_handler\n";
        fprintf fout "\tpushq %%rax\n\tpushq %%rbp\n";
        call_new fout "Bool";
        fprintf fout "\taddq $8, %%rsp\n\tpopq %%rax\n";
        fprintf fout "\tmovq %%rax, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Eq (var, i1, i2) ->
        fprintf fout "\n\t## equality\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i1)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i1));
        fprintf fout "\tmovq %%r14, %%rdi\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i2)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r15\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r15\n"
            (Hashtbl.find envtable (tac_expr_to_name i2));
        fprintf fout "\tmovq %%r15, %%rsi\n";
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tcall eq_handler\n";
        fprintf fout "\tpushq %%rax\n\tpushq %%rbp\n";
        call_new fout "Bool";
        fprintf fout "\taddq $8, %%rsp\n\tpopq %%rax\n";
        fprintf fout "\tmovq %%rax, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_ArithNegate (var, i) ->
        fprintf fout "\n\t## arithmetic negate\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i));
        fprintf fout "\tmovl 24(%%r14), %%edi\n";
        fprintf fout "\tnegl %%edi\n";
        fprintf fout "\tpushq %%rdi\n\tpushq %%rbp\n";
        call_new fout "Int";
        fprintf fout "\taddq $8, %%rsp\n\tpopq %%rdi\n";
        fprintf fout "\tmovl %%edi, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_BoolNegate (var, i) ->
        fprintf fout "\n\t## boolean not\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i)) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i));
        fprintf fout "\tmovq 24(%%r14), %%r14\n";
        fprintf fout "\txorq $1, %%r14\n";
        fprintf fout "\tpushq %%r14\n\tpushq %%rbp\n";
        call_new fout "Bool";
        fprintf fout "\taddq $8, %%rsp\n\tpopq %%r14\n";
        fprintf fout "\tmovq %%r14, 24(%%r13)\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_NullCheck (var, i) ->
        fprintf fout "\n\t## isvoid\n";
        if not (Hashtbl.mem envtable (tac_expr_to_name i)) then (
          stackOffset := !stackOffset + 16;
          (* pop top of stack *)
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i));
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\tpushq %%rbp\n";
        fprintf fout "\tmovq %%r14, %%rdi\n";
        fprintf fout "\tcall is_void\n";
        fprintf fout "\tpopq %%rbp\n";
        fprintf fout "\tpopq %%r12\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Static_FunctionCall (var, mname, cname, args_vars) ->
        (* assume caller object is already on top of the stack? or maybe its vars, need to check *)
        (* TODO: Add a VOID check for var, if caller is void then do a runtime error*)
        fprintf fout "\n\t## %s <- call %s(...) - Static Dispatch\n" var mname;
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\tpushq %%rbp\n";
        stackOffset := !stackOffset + 16;
        (* caller object is on top of the stack*)
        fprintf fout "\tmovq %d(%%rbp), %%r12\n" !stackOffset;

        (* void check *)
        fprintf fout "\tcmpq $0, %%r12\n";
        fprintf fout "\tjne void_good_%d\n" !voidCounter;

        (* use cooloutstr to error out *)
        fprintf fout "\tmovq $%s, %%rdi\n"
          ("voidErrString" ^ string_of_int !voidCounter);
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tcall cooloutstr\n";
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tmovl $1, %%edi\n";
        fprintf fout "\tcall exit\n";

        fprintf fout ".globl void_good_%d\nvoid_good_%d:\n" !voidCounter
          !voidCounter;
        voidCounter := !voidCounter + 1;
        if List.length args_vars mod 2 = 0 then fprintf fout "\tpushq %%rbp\n";
        (*pushing dummy value to maintain 16-byte aligned stack*)
        List.iteri
          (fun i var ->
            let var = tac_expr_to_name var in
            if not (Hashtbl.mem envtable var) then (
              stackOffset := !stackOffset + 16;
              fprintf fout "\tpushq %d(%%rbp)\n" !stackOffset)
            else fprintf fout "\tpushq %s\n" (Hashtbl.find envtable var))
          args_vars;
        let vtableOffset = Hashtbl.find vtable (cname, mname) in
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\t## call %s.%s (vt+%d)\n" cname mname vtableOffset;
        fprintf fout "\tmovq $%s..vtable, %%r14\n" cname;
        fprintf fout "\tmovq %d(%%r14), %%r14\n" vtableOffset;
        fprintf fout "\tcall *%%r14\n";
        fprintf fout "\taddq $%d, %%rsp\n"
          (8
          + (8 * List.length args_vars)
          + (8 * if List.length args_vars mod 2 = 0 then 1 else 0));
        fprintf fout "\tpopq %%rbp\n";
        fprintf fout "\tpopq %%r12\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Dynamic_FunctionCall (var, mname, cname, args_vars) ->
        (* assume caller object is already on top of the stack? or maybe its vars, need to check *)
        (* TODO: Add a VOID check for var, if caller is void then do a runtime error*)
        fprintf fout "\n\t## %s <- call %s(...) - Dynamic Dispatch\n" var mname;
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\tpushq %%rbp\n";
        stackOffset := !stackOffset + 16;
        (* caller object is on top of the stack *)
        fprintf fout "\tmovq %d(%%rbp), %%r12\n" !stackOffset;

        (* void check *)
        fprintf fout "\tcmpq $0, %%r12\n";
        fprintf fout "\tjne void_good_%d\n" !voidCounter;

        (* use cooloutstr to error out *)
        fprintf fout "\tmovq $%s, %%rdi\n"
          ("voidErrString" ^ string_of_int !voidCounter);
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tcall cooloutstr\n";
        fprintf fout "\tandq $-16, %%rsp\n";
        fprintf fout "\tmovl $1, %%edi\n";
        fprintf fout "\tcall exit\n";

        fprintf fout ".globl void_good_%d\nvoid_good_%d:\n" !voidCounter
          !voidCounter;
        voidCounter := !voidCounter + 1;
        if List.length args_vars mod 2 = 0 then fprintf fout "\tpushq %%rbp\n";
        (*pushing dummy value to maintain 16-byte aligned stack*)
        List.iteri
          (fun i var ->
            let var = tac_expr_to_name var in
            if not (Hashtbl.mem envtable var) then (
              stackOffset := !stackOffset + 16;
              fprintf fout "\tpushq %d(%%rbp)\n" !stackOffset)
            else fprintf fout "\tpushq %s\n" (Hashtbl.find envtable var))
          args_vars;
        let vtableOffset = Hashtbl.find vtable (cname, mname) in
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\t## call %s.%s (vt+%d)\n" cname mname vtableOffset;
        fprintf fout "\tmovq 16(%%r12), %%r14\n";
        fprintf fout "\tmovq %d(%%r14), %%r14\n" vtableOffset;
        fprintf fout "\tcall *%%r14\n";
        fprintf fout "\taddq $%d, %%rsp\n"
          (8
          + (8 * List.length args_vars)
          + (8 * if List.length args_vars mod 2 = 0 then 1 else 0));
        fprintf fout "\tpopq %%rbp\n";
        fprintf fout "\tpopq %%r12\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Self_FunctionCall (var, mname, cname, args_vars) ->
        fprintf fout "\n\t## %s <- call %s(...) - Self Dispatch\n" var mname;
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\tpushq %%rbp\n";
        if List.length args_vars mod 2 = 0 then fprintf fout "\tpushq %%rbp\n";
        (*pushing dummy value to maintain 16-byte aligned stack*)
        List.iteri
          (fun i var ->
            let var = tac_expr_to_name var in
            if not (Hashtbl.mem envtable var) then (
              stackOffset := !stackOffset + 16;
              fprintf fout "\tpushq %d(%%rbp)\n" !stackOffset)
            else fprintf fout "\tpushq %s\n" (Hashtbl.find envtable var))
          args_vars;
        let vtableOffset = Hashtbl.find vtable (cname, mname) in
        fprintf fout "\tpushq %%r12\n";
        fprintf fout "\t## call %s.%s (vt+%d)\n" cname mname vtableOffset;
        fprintf fout "\tmovq 16(%%r12), %%r14\n";
        fprintf fout "\tmovq %d(%%r14), %%r14\n" vtableOffset;
        fprintf fout "\tcall *%%r14\n";
        fprintf fout "\taddq $%d, %%rsp\n"
          (8
          + (8 * List.length args_vars)
          + (8 * if List.length args_vars mod 2 = 0 then 1 else 0));
        fprintf fout "\tpopq %%rbp\n";
        fprintf fout "\tpopq %%r12\n";
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_New (var, name) ->
        fprintf fout "\n\t## new object\n";
        if name = "SELF_TYPE" then (
          fprintf fout "\n\t## new SELF_TYPE\n";
          fprintf fout "\tmovq %%r12, %%rdi\n";
          fprintf fout "\tcall self_type_handler\n"
          (* fprintf fout "\taddq $8, %%rsp\n"; *))
        else call_new fout name;
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        stackOffset := !stackOffset - 16
    | TAC_Assign_Default (var, name) ->
        fprintf fout "\n\t## %s <- default %s\n" var name;
        (match name with
        | "Int" | "Bool" | "String" -> call_new fout name
        | _ -> fprintf fout "\tmovq $0, %%r13\n");
        fprintf fout "\tmovq %%r13, %d(%%rbp)\n" !stackOffset;
        Hashtbl.add envtable var (sprintf "%d(%%rbp)" !stackOffset);
        stackOffset := !stackOffset - 16
    | TAC_Assign_Assign (var, i) ->
        fprintf fout "\n\n## TAC_Assign_Assign\n";
        fprintf fout "\n\t## update identifier %s\n" var;
        if not (Hashtbl.mem envtable (tac_expr_to_name i)) then (
          stackOffset := !stackOffset + 16;
          (* pop top of stack *)
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" !stackOffset)
        else
          fprintf fout "\tmovq %s, %%r14\n"
            (Hashtbl.find envtable (tac_expr_to_name i));
        if Hashtbl.mem envtable var then
          fprintf fout "\tmovq %%r14, %s\n" (Hashtbl.find envtable var)
    | TAC_Branch_True (cond, label) ->
        fprintf fout "\n\t## conditional jump\n";
        if not (Hashtbl.mem envtable cond) then (
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %d(%%rbp), %%r13\n" !stackOffset)
        else fprintf fout "\tmovq %s, %%r13\n" (Hashtbl.find envtable cond);
        fprintf fout "\tcmpq $0, 24(%%r13)\n";
        fprintf fout "\tje %s\n" label
    | TAC_Comment comment -> fprintf fout "\n## %s\n" comment
    | TAC_Jump label -> fprintf fout "\tjmp %s\n" label
    | TAC_Label label ->
        fprintf fout ".globl %s\n" label;
        fprintf fout "%s:\n" label
    | TAC_Return label -> fprintf fout "ret\n"
    | TAC_Remove_Let var ->
        fprintf fout "\n\t## Propagating Let return down\n";
        if !stackOffset <= -32 then (
          fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!stackOffset + 16);
          stackOffset := !stackOffset + 16;
          fprintf fout "\tmovq %%r14, %d(%%rbp)\n" (!stackOffset + 16))
    | TAC_Case (var, i, caseList, tacList) ->
        if Hashtbl.mem envtable i then
          fprintf fout "\tmovq %s, %%r13\n" (Hashtbl.find envtable i)
        else
          (* stackOffset := !stackOffset + 16; *)
          fprintf fout "\tmovq %d(%%rbp), %%r13\n" (!stackOffset + 16);
        let branchLabels : (string, string) Hashtbl.t = Hashtbl.create 255 in
        let voidLabel = fresh_label "case" "void" in
        let branchTypes =
          List.map (fun (Case_Elem (_, (_, stype), _)) -> stype) caseList
        in
        List.iter
          (fun x -> Hashtbl.add branchLabels x (fresh_label "case" x))
          branchTypes;
        let errorLabel = fresh_label "case" "error" in
        let endLabel = fresh_label "case" "end" in
        let rec addLabel (tname : label) (prevLabel : label) =
          let nextLabel =
            if not (List.mem tname branchTypes) then (
              Hashtbl.add branchLabels tname prevLabel;
              prevLabel)
            else Hashtbl.find branchLabels tname
          in
          List.iter
            (fun x -> addLabel x nextLabel)
            (Hashtbl.find_all inheritance tname)
        in
        addLabel "Object" errorLabel;

        (* do a void check on i *)
        fprintf fout "\tcmpq $0, %%r13\n";
        fprintf fout "\tje %s\n" voidLabel;
        (* check for each class tag*)
        fprintf fout "\tmovq 0(%%r13), %%r13\n";
        Hashtbl.iter
          (fun cname ctag ->
            fprintf fout "\tmovq $%d, %%r14\n" ctag;
            fprintf fout "\tcmpq %%r14, %%r13\n";
            fprintf fout "\tje %s\n" (Hashtbl.find branchLabels cname))
          class_tags;
        (* void branch *)
        fprintf fout ".globl %s\n%s:\n" voidLabel voidLabel;
        fprintf fout "\tmovq $%s, %%rdi\n"
          ("voidErrString" ^ string_of_int !voidCounter);
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tcall cooloutstr\n";
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tmovl $1, %%edi\n";
        fprintf fout "\tcall exit\n";
        voidCounter := !voidCounter + 1;
        (* no matching branches *)
        fprintf fout ".globl %s\n%s:\n" errorLabel errorLabel;
        fprintf fout "\tmovq $%s, %%rdi\n"
          ("caseErrString" ^ string_of_int !caseErrorCounter);
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tcall cooloutstr\n";
        (* fprintf fout "\tandq $-16, %%rsp\n"; *)
        fprintf fout "\tmovl $1, %%edi\n";
        fprintf fout "\tcall exit\n";
        caseErrorCounter := !caseErrorCounter + 1;
        (* output code for each branch, starting from the least type first *)
        List.iter2
          (fun (Case_Elem (_, (_, btype), cexp)) nodes ->
            fprintf fout ".globl %s\n%s:\n"
              (Hashtbl.find branchLabels btype)
              (Hashtbl.find branchLabels btype);
            let tacOffset = ref !stackOffset in
            (* List.iter ( fun x ->
              tac_to_asm fout tacOffset x;
            ) tacs; *)
            let temp = !visitedNodes in
            visitedNodes := [];
            output_asm fout tacOffset (Some nodes);
            visitedNodes := temp;
            fprintf fout "\tmovq %d(%%rbp), %%r14\n" (!tacOffset + 16);
            fprintf fout "\tmovq %%r14, %d(%%rbp)\n" !stackOffset;
            fprintf fout "\tjmp %s\n" endLabel)
          caseList tacList;
        stackOffset := !stackOffset - 16;
        fprintf fout ".globl %s\n%s:\n" endLabel endLabel
        (* no branches are picked, output a runtime error*)
    | TAC_End_While(_) ->
      stackOffset := !stackOffset + 16;
      fprintf fout "\t## resetting stackOffset for while loops\n";
    | _ -> fprintf fout ""
  and output_asm fout stackOffset cfgNode =
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
          tac_to_asm fout stackOffset cfgNode.comment;
          tac_to_asm fout stackOffset cfgNode.label;
          List.iter (fun x -> tac_to_asm fout stackOffset x) cfgNode.blocks;
          let trueOffset, falseOffset = (ref !stackOffset, ref !stackOffset) in
          output_asm fout trueOffset cfgNode.true_branch;
          output_asm fout falseOffset cfgNode.false_branch)
  in
  let asmname = Filename.chop_extension fname ^ ".s" in
  let aout = open_out asmname in
  let print_calloc fout nmemb msize =
    fprintf fout "\tmovq $%d, %%rdi\n" nmemb;
    fprintf fout "\tmovq $%d, %%rsi\n" msize;
    fprintf aout "\tandq $-16, %%rsp\n";
    fprintf fout "\tcall calloc\n";
    fprintf fout "\tmovq %%rax, %%r12\n"
  in
  let print_asm_string fout sname content =
    fprintf fout ".globl %s\n" sname;
    fprintf fout "%s:\t" sname;
    fprintf fout "## \"%s\"\n" content;
    String.iter
      (fun c -> fprintf aout ".byte %3d # %c\n" (Char.code c) c)
      content;
    fprintf fout ".byte 0\n\n" (* null terminator *)
  in

  Hashtbl.add asm_strings "%d" "percent.d";
  Hashtbl.add asm_strings "%ld" "percent.ld";
  Hashtbl.add asm_strings "" "the.empty.string";
  Hashtbl.add asm_strings "%s%s" "concat.string";
  Hashtbl.add asm_strings "ERROR: 0: Exception: String.substr out of range\\n"
    "substr.error.string";
  Hashtbl.add asm_strings "abort\\n" "abort.string";

  let class_map, impl_map, parent_map, ast = cltype in
  List.iter
    (fun (child, parent) -> Hashtbl.add inheritance parent child)
    parent_map;
  (* output vtables from impl_map *)
  List.iteri
    (fun i (cname, methods) ->
      fprintf aout ".globl %s..vtable\n%s..vtable:\n" cname cname;
      fprintf aout "\t.quad string%d\n" !stringCounter;
      (* name *)
      (* add class name as a string under the label stringn *)
      Hashtbl.add asm_strings cname ("string" ^ string_of_int !stringCounter);
      stringCounter := !stringCounter + 1;
      fprintf aout "\t.quad %s..new\n" cname;
      (* constructor *)
      List.iteri
        (fun i (mname, _, defclass, _) ->
          Hashtbl.add vtable (cname, mname) ((i + 2) * 8);
          fprintf aout "\t.quad %s.%s\n" defclass mname)
        methods)
    impl_map;

  (* Hashtbl.iter (fun (cname, mname) offset -> 
      printf "%s.%s = %d\n" cname mname offset;  
    ) vtable; *)

  (* output constructors for objects *)
  List.iteri
    (fun i (cname, attrs) ->
      fprintf aout ".globl %s..new\n%s..new:\t\t\t## constructor for %s\n" cname
        cname cname;

      (* create activation record *)
      fprintf aout "\tpushq %%rbp\n";
      fprintf aout "\tmovq %%rsp, %%rbp\n";
      let ntemps =
        List.fold_left
          (fun acc (_, _, aexp) ->
            match aexp with
            | Some aexp -> max acc (numTemps aexp.exp_kind)
            | None -> max acc 0)
          0 attrs
        + 1
      in
      (* Adding 1 as assuming the return value is in a temporary*)
      fprintf aout "\t## stack room for temporaries: %d\n" ntemps;
      fprintf aout "\tsubq $%d, %%rsp\n" (ntemps * 16);
      (* allocate for class tag, obj size, vtable, attrs *)
      let attrs_ct =
        match cname with
        | "Bool" | "Int" | "String" -> 1
        | "Object" | "IO" -> 0
        | _ -> List.length attrs
      in
      let obj_size = attrs_ct + 3 in
      print_calloc aout obj_size 8;

      fprintf aout "\tmovq $%d, 0(%%r12)\n" i;
      (* class tag *)
      Hashtbl.add class_tags cname i;
      fprintf aout "\tmovq $%d, 8(%%r12)\n" obj_size;
      fprintf aout "\tmovq $%s..vtable, 16(%%r12)\n" cname;
      Hashtbl.clear envtable;
      Hashtbl.clear ident_tac;
      Hashtbl.add envtable "self" "%r12";
      (* init attributes -- override for internal methods *)
      (match cname with
      | "Bool" | "Int" -> fprintf aout "\tmovq $0, 24(%%r12)\n"
      | "String" -> fprintf aout "\tmovq $the.empty.string, 24(%%r12)\n"
      | "Object" | "IO" -> fprintf aout ""
      | _ ->
          List.iteri
            (fun i (aname, atype, aexp) ->
              Hashtbl.add ident_tac aname (TAC_Variable aname);
              Hashtbl.add attrLocations cname (aname, 24 + (8 * i));
              Hashtbl.add envtable aname (sprintf "%d(%%r12)" (24 + (8 * i)));

              (* set up default initialization *)
              fprintf aout "\t## self[%d] = %s: %s\n" (3 + i) cname aname;
              match atype with
              | "Bool" | "Int" | "String" ->
                  call_new aout atype;
                  fprintf aout "\tmovq %%r13, %d(%%r12)\n" (24 + (8 * i))
              | _ -> fprintf aout "\tmovq $0, %d(%%r12)\n" (24 + (8 * i)))
            attrs;

          List.iteri
            (fun i (aname, atype, aexp) ->
              match aexp with
              | Some aexp ->
                  (* parse expression *)
                  fprintf aout "\t## self[%d] = %s: %s\n" (3 + i) cname aname;
                  (* Hashtbl.clear envtable; *)
                  let node : cfg_node =
                    {
                      label = TAC_Internal "";
                      comment = TAC_Comment "";
                      blocks = [];
                      true_branch = None;
                      false_branch = None;
                      parent_branches = [];
                    }
                  in
                  currNode := node;
                  visitedNodes := [];
                  let _, _ =
                    tac aexp.exp_kind (fresh_var ()) cname
                      (sprintf "attr_%s" aname)
                  in
                  let stackOffset = ref 0 in
                  output_asm aout stackOffset (Some node);
                  let stackOffset =
                    if !stackOffset = 0 then 0 else !stackOffset + 16
                  in
                  fprintf aout "\tmovq %d(%%rbp), %%r14\n" stackOffset;
                  fprintf aout "\tmovq %%r14, %d(%%r12)\n" (24 + (8 * i))
              | None -> ( (* do nothing since already default initialized *) ))
            attrs);
      fprintf aout "\tmovq %%r12, %%r13\n";
      fprintf aout "\tmovq %%rbp, %%rsp\n";
      fprintf aout "\tpopq %%rbp\n";
      fprintf aout "\tret\n")
    class_map;

  (* output methods for non-default classes *)
  fprintf aout "\n## USER METHOD BODIES BEGINS\n\n";

  let user_impl_map =
    List.filter
      (fun (x, _) ->
        not (List.mem x [ "Object"; "IO"; "Int"; "String"; "Bool" ]))
      impl_map
  in
  List.iteri
    (fun cid (cname, methods) ->
      let non_inherited_methods =
        List.filter (fun (_, _, defname, _) -> cname = defname) methods
      in
      List.iteri
        (fun mid (mname, formals, _, body) ->
          fprintf aout "## method definition of %s.%s\n" cname mname;
          fprintf aout ".globl %s.%s\n" cname mname;
          fprintf aout "%s.%s:\n" cname mname;
          fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";
          fprintf aout "\tmovq 16(%%rbp), %%r12\n";

          (* add hashtbl offset entries for each formal, relative to %rbp *)
          Hashtbl.clear envtable;
          Hashtbl.clear ident_tac;
          Hashtbl.add envtable "self" "%r12";
          List.iter
            (fun (aname, loc) ->
              Hashtbl.add envtable aname (sprintf "%d(%%r12)" loc);
              Hashtbl.add ident_tac aname (TAC_Variable aname))
            (List.rev (Hashtbl.find_all attrLocations cname));
          List.iteri
            (fun i name ->
              fprintf aout "\t## fp[%d] = %s  %d(%%rbp)\n" (3 + i) name
                (24 + (8 * i));
              Hashtbl.add envtable name (sprintf "%d(%%rbp)" (24 + (8 * i)));
              Hashtbl.add ident_tac name (TAC_Variable name))
            formals;
          let ntemps = numTemps body.exp_kind + 1 in
          (* Adding 1 as assuming the return value is in a temporary*)
          fprintf aout "\t## stack room for temporaries: %d\n" ntemps;
          fprintf aout "\tsubq $%d, %%rsp\n" (ntemps * 16);
          let node : cfg_node =
            {
              label = TAC_Internal "";
              comment = TAC_Comment "\tstart";
              blocks = [];
              true_branch = None;
              false_branch = None;
              parent_branches = [];
            }
          in
          currNode := node;
          visitedNodes := [];
          labelCount := 1;
          (* TODO find the AST for the method and then run it *)
          let _, _ = tac body.exp_kind (fresh_var ()) cname mname in
          let stackOffset = ref 0 in
          output_asm aout stackOffset (Some node);
          let stackOffset = if !stackOffset = 0 then 0 else !stackOffset + 16 in
          fprintf aout "\tmovq %d(%%rbp), %%r13\n" stackOffset;
          fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n")
        non_inherited_methods)
    user_impl_map;

  fprintf aout "\n## USER METHOD BODIES ENDS\n";
  fprintf aout "\n## INTERNAL METHOD BODIES BEGINS\n\n";

  (* output internal method bodies - copied from COOL reference compiler *)
  let internal_impl_map =
    List.filter
      (fun (x, _) -> List.mem x [ "Object"; "IO"; "Int"; "String"; "Bool" ])
      impl_map
  in
  List.iteri
    (fun cid (cname, methods) ->
      let non_inherited_methods =
        List.filter (fun (_, _, defname, _) -> cname = defname) methods
      in
      List.iteri
        (fun mid (mname, mformals, mdef, _) ->
          fprintf aout "## method definition of %s.%s\n" mdef mname;
          fprintf aout ".globl %s.%s\n" cname mname;
          fprintf aout "%s.%s:\n" cname mname;
          fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";

          (* modified from reference compiler *)
          (match (cname, mname) with
          | "IO", "in_int" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $8, %%rsp\n";

              call_new aout "Int";
              fprintf aout "\tmovq %%r13, %%r14\n";

              (* allocate input buffer of size 4096 *)
              fprintf aout "\n\t## calloc input buffer, store ptr in stack\n";
              fprintf aout "\tmovl $1, %%esi\n";
              fprintf aout "\tmovl $4096, %%edi\n";
              fprintf aout "\tmovq %%rsp, %%r13\n";
              fprintf aout "\tandq $-16, %%rsp\n";
              fprintf aout "\tcall calloc\n";
              fprintf aout "\tmovq %%r13, %%rsp\n";
              fprintf aout "\tpushq %%rax\n";

              (* read input with fgets into buffer *)
              fprintf aout "\n\t## read input via fgets\n";
              fprintf aout "\tmovq %%rax, %%rdi\n";
              fprintf aout "\tmovq $4096, %%rsi\n";
              fprintf aout "\tmovq stdin(%%rip), %%rdx\n";

              (* 16B align *)
              fprintf aout "\n\t## guarantee 16-byte alignment before call\n";
              fprintf aout "\tandq $-16, %%rsp\n";
              fprintf aout "\tcall fgets\n";

              (* read int *)
              fprintf aout "\n\t## read int\n";
              fprintf aout "\tpopq %%rdi\n";
              fprintf aout "\tmovl $0, %%eax\n";
              fprintf aout "\tpushq %%rax\n";
              fprintf aout "\tmovq $percent.ld, %%rsi\n";
              fprintf aout "\tmovq %%rsp, %%rdx\n";

              (* convert input to long with sscaf %ld *)
              fprintf aout "\n\t## guarantee 16-byte alignment before call\n";
              fprintf aout "\tandq $-16, %%rsp\n";
              fprintf aout "\tcall sscanf\n";
              fprintf aout "\tpopq %%rax\n";

              (* check overflow of 32-bit int boundaries *)
              fprintf aout "\n\t## check overflow\n";
              fprintf aout "\tmovq $0, %%rsi\n";
              fprintf aout "\tcmpq $2147483647, %%rax\n";
              fprintf aout "\tcmovg %%rsi, %%rax\n";
              fprintf aout "\tcmpq $-2147483648, %%rax\n";
              fprintf aout "\tcmovl %%rsi, %%rax\n";

              fprintf aout "\n.in_int_end:\n";
              fprintf aout "\t## store int into Int()\n";
              fprintf aout "\tmovq %%rax, %%r13\n";
              fprintf aout "\tmovq %%r13, 24(%%r14)\n";
              fprintf aout "\tmovq %%r14, %%r13\n"
          | "IO", "in_string" ->
              fprintf aout "\tsubq $16, %%rsp\n";
              call_new aout "String";
              fprintf aout "\tmovq %%r13, %%r14\n";
              fprintf aout "\tandq $-16, %%rsp\n";
              fprintf aout "\tcall coolgetstr\n";
              fprintf aout "\tmovq %%rax, %%r13\n";
              fprintf aout "\tmovq %%r13, 24(%%r14)\n";
              fprintf aout "\tmovq %%r14, %%r13\n"
          | "IO", "out_int" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";
              fprintf aout "\tmovq 24(%%rbp), %%r14\n";
              fprintf aout "\tmovq 24(%%r14), %%r13\n";
              fprintf aout "\tmovq $percent.ld, %%rdi\n";
              fprintf aout "\tmovl %%r13d, %%eax\n";
              fprintf aout "\tcdqe\n";
              fprintf aout "\tmovq %%rax, %%rsi\n";
              fprintf aout "\tmovl $0, %%eax\n";
              fprintf aout "\tandq $-16, %%rsp\n";
              fprintf aout "\tcall printf\n";
              fprintf aout "\tmov %%r12, %%r13\n"
          | "IO", "out_string" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";
              fprintf aout "\tmovq 24(%%rbp), %%r14\n";
              fprintf aout "\tmovq 24(%%r14), %%r13\n";
              fprintf aout "\tmovq %%r13, %%rdi\n";
              fprintf aout "\tandq $-16, %%rsp\n";
              fprintf aout "\tcall cooloutstr\n";
              fprintf aout "\tmovq %%r12, %%r13\n"
          | "Object", "abort" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";
              fprintf aout "\tmovq $abort.string, %%rdi\n";
              fprintf aout "\tcall cooloutstr\n";
              fprintf aout "\tmovl $0, %%edi\n";
              fprintf aout "\tcall exit\n"
          | "Object", "copy" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";

              fprintf aout "\tmovq 8(%%r12), %%r14\n";

              fprintf aout "\tmovq $8, %%rsi\n";
              fprintf aout "\tmovq %%r14, %%rdi\n";
              fprintf aout "\tandq $-16, %%rsp\n";
              fprintf aout "\tcall calloc\n";
              fprintf aout "\tmovq %%rax, %%r13\n";
              fprintf aout "\tpushq %%r13\n";

              (* copy loop *)
              fprintf aout ".globl l1\nl1:\n";
              fprintf aout "\tcmpq $0, %%r14\n";
              fprintf aout "\tje l2\n";
              fprintf aout "\tmovq 0(%%r12), %%r15\n";
              fprintf aout "\tmovq %%r15, 0(%%r13)\n";
              fprintf aout "\tmovq $8, %%r15\n";
              fprintf aout "\taddq %%r15, %%r12\n";
              fprintf aout "\taddq %%r15, %%r13\n";
              fprintf aout "\tmovq $1, %%r15\n";
              fprintf aout "\tsubq %%r15, %%r14\n";
              fprintf aout "\tjmp l1\n";
              fprintf aout ".globl l2\nl2:\n";

              (* done with copy loop *)
              fprintf aout "\tpopq %%r13\n"
          | "Object", "type_name" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";
              fprintf aout "\tpushq %%rbp\n";
              fprintf aout "\tpushq %%r12\n";
              fprintf aout "\tmovq $String..new, %%r14\n";
              fprintf aout "\tcall *%%r14\n";
              fprintf aout "\tpopq %%r12\n";
              fprintf aout "\tpopq %%rbp\n";
              fprintf aout "\tmovq 16(%%r12), %%r14\n";
              fprintf aout "\tmovq 0(%%r14), %%r14\n";
              fprintf aout "\tmovq %%r14, 24(%%r13)\n"
          | "String", "length" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";
              call_new aout "Int";
              fprintf aout "\tmovq %%r13, %%r14\n";
              fprintf aout "\tmovq 24(%%r12), %%r13\n";
              fprintf aout "\tmovq %%r13, %%rdi\n";
              fprintf aout "\tmovl $0, %%eax\n";
              fprintf aout "\tcall coolstrlen\n";
              fprintf aout "\tmovq %%rax, %%r13\n";
              fprintf aout "\tmovq %%r13, 24(%%r14)\n";
              fprintf aout "\tmovq %%r14, %%r13\n"
          | "String", "concat" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";
              fprintf aout "\t\n## init new string for return\n";
              call_new aout "String";
              fprintf aout "\tmovq %%r13, %%r15\n";
              fprintf aout "\tmovq 24(%%rbp), %%r14\n";
              fprintf aout "\tmovq 24(%%r14), %%r14\n";
              fprintf aout "\tmovq 24(%%r12), %%r13\n";

              fprintf aout "\t\n## rdi is caller (LHS)\n";
              fprintf aout "\tmovq %%r13, %%rdi\n";
              fprintf aout "\tmovq %%r14, %%rsi\n";
              fprintf aout "\tcall coolstrcat\n";
              fprintf aout "\tmovq %%rax, %%r13\n";
              fprintf aout "\tmovq %%r13, 24(%%r15)\n";
              fprintf aout "\tmovq %%r15, %%r13\n"
          | "String", "substr" ->
              fprintf aout "\tmovq 16(%%rbp), %%r12\n";
              fprintf aout "\tsubq $16, %%rsp\n";
              fprintf aout "\tpushq %%rbp\n";
              fprintf aout "\tpushq %%r12\n";
              fprintf aout "\tmovq $String..new, %%r14\n";
              fprintf aout "\tcall *%%r14\n";
              fprintf aout "\tpopq %%r12\n";
              fprintf aout "\tpopq %%rbp\n";
              fprintf aout "\tmovq %%r13, %%r15\n";
              fprintf aout "\tmovq 32(%%rbp), %%r14\n";
              fprintf aout "\tmovq 24(%%r14), %%r14\n";
              fprintf aout "\tmovq 24(%%rbp), %%r13\n";
              fprintf aout "\tmovq 24(%%r13), %%r13\n";
              fprintf aout "\tmovq 24(%%r12), %%r12\n";
              fprintf aout "\tmovq %%r12, %%rdi\n";
              fprintf aout "\tmovq %%r13, %%rsi ## start index\n ";
              fprintf aout "\tmovq %%r14, %%rdx ## length\n ";
              fprintf aout "\tcall coolsubstr\n";
              fprintf aout "\tmovq %%rax, %%r13\n";
              fprintf aout "\tcmpq $0, %%r13\n";
              fprintf aout "\tjne l3\n";
              fprintf aout "\tmovq $substr.error.string, %%r13\n";
              fprintf aout "\tmovq %%r13, %%rdi\n";
              fprintf aout "\tcall cooloutstr\n";
              fprintf aout "\tmovl $0, %%edi\n";
              fprintf aout "\tcall exit\n";
              fprintf aout ".globl l3\n";
              fprintf aout "l3:\n";
              fprintf aout "\tmovq %%r13, 24(%%r15)\n";
              fprintf aout "\tmovq %%r15, %%r13\n"
          | _ ->
              fprintf aout "\n## MISSING INTERNAL METHOD DEF for %s.%s\n\n"
                cname mname);

          fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n")
        non_inherited_methods)
    internal_impl_map;

  (* cooloutstr - copied from ref compiler *)
  fprintf aout "\n.globl cooloutstr\n";
  fprintf aout "cooloutstr:\n";
  fprintf aout "\tpushq\t%%rbp\n";
  fprintf aout "\tmovq\t%%rsp, %%rbp\n";
  fprintf aout "\tsubq\t$32, %%rsp\n";
  fprintf aout "\tmovq\t%%rdi, -24(%%rbp)\n";
  fprintf aout "\tmovl\t$0, -4(%%rbp)\n";
  fprintf aout "\tjmp\t.L2\n";
  fprintf aout ".L5:\n";
  fprintf aout "\tmovl\t-4(%%rbp), %%eax\n";
  fprintf aout "\tmovslq\t%%eax, %%rdx\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rdx, %%rax\n";
  fprintf aout "\tmovzbl\t(%%rax), %%eax\n";
  fprintf aout "\tcmpb\t$92, %%al\n";
  fprintf aout "\tjne\t.L3\n";
  fprintf aout "\tmovl\t-4(%%rbp), %%eax\n";
  fprintf aout "\tcltq\n";
  fprintf aout "\tleaq\t1(%%rax), %%rdx\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rdx, %%rax\n";
  fprintf aout "\tmovzbl\t(%%rax), %%eax\n";
  fprintf aout "\tcmpb\t$110, %%al\n";
  fprintf aout "\tjne\t.L3\n";
  fprintf aout "\tmovq\tstdout(%%rip), %%rax\n";
  fprintf aout "\tmovq\t%%rax, %%rsi\n";
  fprintf aout "\tmovl\t$10, %%edi\n";
  fprintf aout "\tcall\tfputc\n";
  fprintf aout "\taddl\t$2, -4(%%rbp)\n";
  fprintf aout "\tjmp\t.L2\n";
  fprintf aout ".L3:\n";
  fprintf aout "\tmovl\t-4(%%rbp), %%eax\n";
  fprintf aout "\tmovslq\t%%eax, %%rdx\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rdx, %%rax\n";
  fprintf aout "\tmovzbl\t(%%rax), %%eax\n";
  fprintf aout "\tcmpb\t$92, %%al\n";
  fprintf aout "\tjne\t.L4\n";
  fprintf aout "\tmovl\t-4(%%rbp), %%eax\n";
  fprintf aout "\tcltq\n";
  fprintf aout "\tleaq\t1(%%rax), %%rdx\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rdx, %%rax\n";
  fprintf aout "\tmovzbl\t(%%rax), %%eax\n";
  fprintf aout "\tcmpb\t$116, %%al\n";
  fprintf aout "\tjne\t.L4\n";
  fprintf aout "\tmovq\tstdout(%%rip), %%rax\n";
  fprintf aout "\tmovq\t%%rax, %%rsi\n";
  fprintf aout "\tmovl\t$9, %%edi\n";
  fprintf aout "\tcall\tfputc\n";
  fprintf aout "\taddl\t$2, -4(%%rbp)\n";
  fprintf aout "\tjmp\t.L2\n";
  fprintf aout ".L4:\n";
  fprintf aout "\tmovq\tstdout(%%rip), %%rdx\n";
  fprintf aout "\tmovl\t-4(%%rbp), %%eax\n";
  fprintf aout "\tmovslq\t%%eax, %%rcx\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rcx, %%rax\n";
  fprintf aout "\tmovzbl\t(%%rax), %%eax\n";
  fprintf aout "\tmovsbl\t%%al, %%eax\n";
  fprintf aout "\tmovq\t%%rdx, %%rsi\n";
  fprintf aout "\tmovl\t%%eax, %%edi\n";
  fprintf aout "\tcall\tfputc\n";
  fprintf aout "\taddl\t$1, -4(%%rbp)\n";
  fprintf aout ".L2:\n";
  fprintf aout "\tmovl\t-4(%%rbp), %%eax\n";
  fprintf aout "\tmovslq\t%%eax, %%rdx\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rdx, %%rax\n";
  fprintf aout "\tmovzbl\t(%%rax), %%eax\n";
  fprintf aout "\ttestb\t%%al, %%al\n";
  fprintf aout "\tjne\t.L5\n";
  fprintf aout "\tmovq\tstdout(%%rip), %%rax\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tfflush\n";
  fprintf aout "\tnop\n";
  fprintf aout "\tleave\n";
  fprintf aout "\tret\n";
  fprintf aout ".LFE6:\n";
  fprintf aout "\t.size\tcooloutstr, .-cooloutstr\n";

  (* coolstrlen - copied from ref compiler *)
  fprintf aout "\n.globl coolstrlen\n";
  fprintf aout "coolstrlen:\n";
  fprintf aout ".LFB7:\n";
  fprintf aout "\tpushq\t%%rbp\n";
  fprintf aout "\tmovq %%rsp, %%rbp\n";
  fprintf aout "\tmovq %%rdi, -24(%%rbp)\n";
  fprintf aout "\tmovl $0, -4(%%rbp)\n";
  fprintf aout "\tjmp .L7\n";
  fprintf aout ".L8:\n";
  fprintf aout "\tmovl -4(%%rbp), %%eax\n";
  fprintf aout "\taddl $1, %%eax\n";
  fprintf aout "\tmovl %%eax, -4(%%rbp)\n";
  fprintf aout ".L7:\n";
  fprintf aout "\tmovl -4(%%rbp), %%eax\n";
  fprintf aout "\tmovl %%eax, %%edx\n";
  fprintf aout "\tmovq -24(%%rbp), %%rax\n";
  fprintf aout "\taddq %%rdx, %%rax\n";
  fprintf aout "\tmovzbl (%%rax), %%eax\n";
  fprintf aout "\ttestb %%al, %%al\n";
  fprintf aout "\tjne .L8\n";
  fprintf aout "\tmovl -4(%%rbp), %%eax\n";
  fprintf aout "\tpopq %%rbp\n";
  fprintf aout "\tret\n";
  fprintf aout ".LFE7:\n";

  (* coolstrcat - modified from ref compiler *)
  fprintf aout "\n.globl\tcoolstrcat\n";
  fprintf aout "coolstrcat:\n";
  fprintf aout "\tpushq\t%%rbp\n";
  fprintf aout "\tmovq\t%%rsp, %%rbp\n";
  fprintf aout "\tpushq\t%%rbx\n";
  fprintf aout "\tsubq\t$40, %%rsp\n";
  fprintf aout "\tmovq\t%%rdi, -40(%%rbp)\n";
  fprintf aout "\tmovq\t%%rsi, -48(%%rbp)\n";
  fprintf aout "\tcmpq\t$0, -40(%%rbp)\n";
  fprintf aout "\tjne\t.L11\n";
  fprintf aout "\tmovq\t-48(%%rbp), %%rax\n";
  fprintf aout "\tjmp\t.L12\n";
  fprintf aout ".L11:\n";
  fprintf aout "\tcmpq\t$0, -48(%%rbp)\n";
  fprintf aout "\tjne\t.L13\n";
  fprintf aout "\tmovq\t-40(%%rbp), %%rax\n";
  fprintf aout "\tjmp\t.L12\n";
  fprintf aout ".L13:\n";
  fprintf aout "\t## calculate return string length\n";
  fprintf aout "\tmovq\t-40(%%rbp), %%rax\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tcoolstrlen\n";
  fprintf aout "\tmovl\t%%eax, %%ebx\n";
  fprintf aout "\tmovq\t-48(%%rbp), %%rax\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tcoolstrlen\n";
  fprintf aout "\taddl\t%%ebx, %%eax\n";
  fprintf aout "\taddl\t$1, %%eax\n";
  fprintf aout "\t\n";
  fprintf aout "\t## store string length at -32\n";
  fprintf aout "\tmovl\t%%eax, -32(%%rbp)\n";
  fprintf aout "\tmovl\t-32(%%rbp), %%eax\n";
  fprintf aout "\tcltq\n";
  fprintf aout "\t\n";
  fprintf aout "\t## allocate new string\n";
  fprintf aout "\tmovq\t$1, %%rsi\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tcalloc\n";
  fprintf aout "\tmovq\t%%rax, -24(%%rbp) ## store char ptr at -24\n";
  fprintf aout "\t\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rdi\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rsi\n";
  fprintf aout "\tmovq\t$concat.string, %%rdx\n";
  fprintf aout "\tmovq\t-40(%%rbp), %%rcx\n";
  fprintf aout "\tmovq\t-48(%%rbp), %%r8\n";
  fprintf aout "\t\n";
  fprintf aout "\tmovl\t$0, %%eax\n";
  fprintf aout "\tandq\t$-16, %%rsp\n";
  fprintf aout "\tcall\tsnprintf\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout ".L12:\n";
  fprintf aout "\tmovq -8(%%rbp), %%rbx\n";
  fprintf aout "\tleave\n";
  fprintf aout "\tret\n";

  (* coolgetstr - copied from ref compiler *)
  fprintf aout ".globl\tcoolgetstr\n";
  fprintf aout "coolgetstr:\n";
  fprintf aout "\tpushq\t%%rbp\n";
  fprintf aout "\tmovq\t%%rsp, %%rbp\n";
  fprintf aout "\tsubq\t$32, %%rsp\n";
  fprintf aout "\tmovq\t%%fs:40, %%rax\n";
  fprintf aout "\tmovq\t%%rax, -8(%%rbp)\n";
  fprintf aout "\txorl\t%%eax, %%eax\n";
  fprintf aout "\tmovq\t$0, -32(%%rbp)\n";
  fprintf aout "\tmovq\t$0, -24(%%rbp)\n";
  fprintf aout "\tmovq\tstdin(%%rip), %%rdx\n";
  fprintf aout "\tleaq\t-24(%%rbp), %%rcx\n";
  fprintf aout "\tleaq\t-32(%%rbp), %%rax\n";
  fprintf aout "\tmovq\t%%rcx, %%rsi\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tgetline\n";
  fprintf aout "\tmovq\t%%rax, -16(%%rbp)\n";
  fprintf aout "\tcmpq\t$-1, -16(%%rbp)\n";
  fprintf aout "\tje\t.L15\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rax\n";
  fprintf aout "\ttestq\t%%rax, %%rax\n";
  fprintf aout "\tjne\t.L16\n";
  fprintf aout ".L15:\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rax\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tfree\n";
  fprintf aout "\tmovl\t$1, %%edi\n";
  fprintf aout "\tcall\tmalloc\n";
  fprintf aout "\tmovq\t%%rax, -32(%%rbp)\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rax\n";
  fprintf aout "\tmovb\t$0, (%%rax)\n";
  fprintf aout "\tjmp\t.L17\n";
  fprintf aout ".L16:\n";
  fprintf aout "\tmovq\t-16(%%rbp), %%rdx\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rax\n";
  fprintf aout "\tmovl\t$0, %%esi\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tmemchr\n";
  fprintf aout "\ttestq\t%%rax, %%rax\n";
  fprintf aout "\tje\t.L18\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rax\n";
  fprintf aout "\tmovb\t$0, (%%rax)\n";
  fprintf aout "\tjmp\t.L17\n";
  fprintf aout ".L18:\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rdx\n";
  fprintf aout "\tmovq\t-16(%%rbp), %%rax\n";
  fprintf aout "\tsubq\t$1, %%rax\n";
  fprintf aout "\taddq\t%%rdx, %%rax\n";
  fprintf aout "\tmovzbl\t(%%rax), %%eax\n";
  fprintf aout "\tcmpb\t$10, %%al\n";
  fprintf aout "\tjne\t.L17\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rdx\n";
  fprintf aout "\tsubq\t$1, -16(%%rbp)\n";
  fprintf aout "\tmovq\t-16(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rdx, %%rax\n";
  fprintf aout "\tmovb\t$0, (%%rax)\n";
  fprintf aout ".L17:\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rax\n";
  fprintf aout "\tmovq\t-8(%%rbp), %%rdx\n";
  fprintf aout "\tsubq\t%%fs:40, %%rdx\n";
  fprintf aout "\tje\t.L20\n";
  fprintf aout "\tcall\t__stack_chk_fail\n";
  fprintf aout ".L20:\n";
  fprintf aout "\tleave\n";
  fprintf aout "\tret\n";

  (* coolsubstr - copied from ref compiler *)
  fprintf aout ".globl\tcoolsubstr\n";
  fprintf aout "coolsubstr:\n";
  fprintf aout ".LFB10:\n";
  fprintf aout "\tpushq\t%%rbp\n";
  fprintf aout "\tmovq\t%%rsp, %%rbp\n";
  fprintf aout "\tsubq\t$48, %%rsp\n";
  fprintf aout "\tmovq\t%%rdi, -24(%%rbp)\n";
  fprintf aout "\tmovq\t%%rsi, -32(%%rbp)\n";
  fprintf aout "\tmovq\t%%rdx, -40(%%rbp)\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rax\n";
  fprintf aout "\tmovq\t%%rax, %%rdi\n";
  fprintf aout "\tcall\tcoolstrlen\n";
  fprintf aout "\tmovl\t%%eax, -4(%%rbp)\n";
  fprintf aout "\tcmpq\t$0, -32(%%rbp)\n";
  fprintf aout "\tjs\t.L22\n";
  fprintf aout "\tcmpq\t$0, -40(%%rbp)\n";
  fprintf aout "\tjs\t.L22\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rdx\n";
  fprintf aout "\tmovq\t-40(%%rbp), %%rax\n";
  fprintf aout "\taddq\t%%rax, %%rdx\n";
  fprintf aout "\tmovl\t-4(%%rbp), %%eax\n";
  fprintf aout "\tcltq\n";
  fprintf aout "\tcmpq\t%%rax, %%rdx\n";
  fprintf aout "\tjle\t.L23\n";
  fprintf aout ".L22:\n";
  fprintf aout "\tmovl\t$0, %%eax\n";
  fprintf aout "\tjmp\t.L24\n";
  fprintf aout ".L23:\n";
  fprintf aout "\tmovq\t-40(%%rbp), %%rax\n";
  fprintf aout "\tmovq\t-32(%%rbp), %%rcx\n";
  fprintf aout "\tmovq\t-24(%%rbp), %%rdx\n";
  fprintf aout "\taddq\t%%rcx, %%rdx\n";
  fprintf aout "\tmovq\t%%rax, %%rsi\n";
  fprintf aout "\tmovq\t%%rdx, %%rdi\n";
  fprintf aout "\tcall\tstrndup\n";
  fprintf aout ".L24:\n";
  fprintf aout "\tleave\n";
  fprintf aout "\tret\n";
  fprintf aout ".LFE10:\n";

  fprintf aout "\n## INTERNAL METHOD BODIES END\n";

  (* print out string constants *)
  Hashtbl.iter
    (fun cname sname -> print_asm_string aout sname cname)
    asm_strings;

  (* print out less than handler *)
  fprintf aout "\n## LT_HANDLER\n";
  fprintf aout ".globl lt_handler\nlt_handler:\n";
  fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";

  (* void checks *)
  fprintf aout "\n\t## void checks\n";
  fprintf aout "\tcmpq $0, %%rdi\n";
  fprintf aout "\tje lt_false\n";
  fprintf aout "\tcmpq $0, %%rsi\n";
  fprintf aout "\tje lt_false\n";

  (* load class tags *)
  fprintf aout "\n\t## load class tags\n";
  fprintf aout "\tmovq 0(%%rdi), %%r13\n";
  fprintf aout "\tmovq 0(%%rsi), %%r14\n";

  (* jump to correct compare for class *)
  fprintf aout "\n\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Bool");
  fprintf aout "\tje lt_bool\n";
  fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Int");
  fprintf aout "\tje lt_int\n";
  fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "String");
  fprintf aout "\tje lt_string\n";
  fprintf aout "\tjmp lt_false\n";

  (* boolean comparisons *)
  fprintf aout "\n\t.globl lt_bool\n\tlt_bool:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Bool");
  fprintf aout "\tjne lt_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
  fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
  fprintf aout "\tcmpl %%edi, %%esi\n";
  fprintf aout "\tjl lt_true\n";
  fprintf aout "\tjmp lt_false\n";

  (* int comparisons *)
  fprintf aout "\n\t.globl lt_int\n\tlt_int:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Int");
  fprintf aout "\tjne lt_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
  fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
  fprintf aout "\tcmpl %%edi, %%esi\n";
  fprintf aout "\tjl lt_true\n";
  fprintf aout "\tjmp lt_false\n";

  (* string comparison *)
  fprintf aout "\n\t.globl lt_string\n\tlt_string:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "String");
  fprintf aout "\tjne lt_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%r15\n";
  fprintf aout "\tmovq 24(%%rsi), %%rdi\n";
  fprintf aout "\tmovq %%r15, %%rsi\n";
  fprintf aout "\tandq $-16, %%rsp\n";
  fprintf aout "\tcall strcmp\n";
  fprintf aout "\tcmpl $0, %%eax\n";
  fprintf aout "\tjl lt_true\n";
  fprintf aout "\tjmp lt_false\n";

  fprintf aout "\n.globl lt_false\nlt_false:\n";
  fprintf aout "\tmovq $0, %%rax\n";
  fprintf aout "\tjmp lt_handler_end\n";
  fprintf aout "\n.globl lt_true\nlt_true:\n";
  fprintf aout "\tmovq $1, %%rax\n";
  fprintf aout "\tjmp lt_handler_end\n";
  fprintf aout "\n.globl lt_handler_end\nlt_handler_end:\n";
  fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";

  (* print out less than or equal to handler *)
  fprintf aout "\n## LE_HANDLER\n";
  fprintf aout ".globl le_handler\nle_handler:\n";
  fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";

  (* void checks *)
  fprintf aout "\n\t## void checks\n";
  fprintf aout "\tcmpq $0, %%rdi\n";
  fprintf aout "\tje le_false\n";
  fprintf aout "\tcmpq $0, %%rsi\n";
  fprintf aout "\tje le_false\n";

  (* compare pointers *)
  fprintf aout "\n\t## compare pointers\n";
  fprintf aout "\tcmpq %%rdi, %%rsi\n";
  fprintf aout "\tje le_true\n";

  (* load class tags *)
  fprintf aout "\n\t## load class tags\n";
  fprintf aout "\tmovq 0(%%rdi), %%r13\n";
  fprintf aout "\tmovq 0(%%rsi), %%r14\n";

  (* jump to correct compare for class *)
  fprintf aout "\n\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Bool");
  fprintf aout "\tje le_bool\n";
  fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Int");
  fprintf aout "\tje le_int\n";
  fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "String");
  fprintf aout "\tje le_string\n";
  fprintf aout "\tjmp le_false\n";

  (* boolean comparisons *)
  fprintf aout "\n\t.globl le_bool\n\tle_bool:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Bool");
  fprintf aout "\tjne le_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
  fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
  fprintf aout "\tcmpl %%edi, %%esi\n";
  fprintf aout "\tjle le_true\n";
  fprintf aout "\tjmp le_false\n";

  (* int comparisons *)
  fprintf aout "\n\t.globl le_int\n\tle_int:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Int");
  fprintf aout "\tjne le_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
  fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
  fprintf aout "\tcmpl %%edi, %%esi\n";
  fprintf aout "\tjle le_true\n";
  fprintf aout "\tjmp le_false\n";

  (* string comparison *)
  fprintf aout "\n\t.globl le_string\n\tle_string:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "String");
  fprintf aout "\tjne le_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%r15\n";
  fprintf aout "\tmovq 24(%%rsi), %%rdi\n";
  fprintf aout "\tmovq %%r15, %%rsi\n";
  fprintf aout "\tandq $-16, %%rsp\n";
  fprintf aout "\tcall strcmp\n";
  fprintf aout "\tcmp $0, %%eax\n";
  fprintf aout "\tjle le_true\n";
  fprintf aout "\tjmp le_false\n";

  fprintf aout "\n.globl le_false\nle_false:\n";
  fprintf aout "\tmovq $0, %%rax\n";
  fprintf aout "\tjmp le_handler_end\n";
  fprintf aout "\n.globl le_true\nle_true:\n";
  fprintf aout "\tmovq $1, %%rax\n";
  fprintf aout "\n\tjmp le_handler_end\n";
  fprintf aout ".globl le_handler_end\nle_handler_end:\n";
  fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";

  (* print out equal handler *)
  fprintf aout "## EQ_HANDLER\n";
  fprintf aout ".globl eq_handler\neq_handler:\n";
  fprintf aout "\tpushq %%rbp\n";
  fprintf aout "\tmovq %%rsp, %%rbp\n";

  (* compare pointers *)
  fprintf aout "\n\t## compare pointers\n";
  fprintf aout "\tcmpq %%rdi, %%rsi\n";
  fprintf aout "\tje eq_true\n";

  (* check if either is void -> false *)
  (* void equality is covered by pointer check *)
  fprintf aout "\n\t## compare void\n";
  fprintf aout "\tcmpq $0, %%rdi\n";
  fprintf aout "\tje eq_false\n";
  fprintf aout "\tcmpq $0, %%rsi\n";
  fprintf aout "\tje eq_false\n";

  (* load class tags *)
  fprintf aout "\n\t## load class tags\n";
  fprintf aout "\tmovq 0(%%rdi), %%r13\n";
  fprintf aout "\tmovq 0(%%rsi), %%r14\n";

  (* jump to correct compare for class *)
  fprintf aout "\n\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Bool");
  fprintf aout "\tje eq_bool\n";
  fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "Int");
  fprintf aout "\tje eq_int\n";
  fprintf aout "\tcmpq $%d, %%r13\n" (Hashtbl.find class_tags "String");
  fprintf aout "\tje eq_string\n";
  fprintf aout "\tjmp eq_false\n";

  (* boolean comparisons *)
  fprintf aout "\n\t.globl eq_bool\n\teq_bool:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Bool");
  fprintf aout "\tjne eq_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
  fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
  fprintf aout "\tcmpl %%edi, %%esi\n";
  fprintf aout "\tje eq_true\n";
  fprintf aout "\tjmp eq_false\n";

  (* int comparisons *)
  fprintf aout "\n\t.globl eq_int\n\teq_int:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "Int");
  fprintf aout "\tjne eq_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
  fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
  fprintf aout "\tcmpl %%edi, %%esi\n";
  fprintf aout "\tje eq_true\n";
  fprintf aout "\tjmp eq_false\n";

  (* string comparison *)
  fprintf aout "\n\t.globl eq_string\n";
  fprintf aout "\teq_string:\n";
  fprintf aout "\tcmpq $%d, %%r14\n" (Hashtbl.find class_tags "String");
  fprintf aout "\tjne eq_false\n";
  fprintf aout "\tmovq 24(%%rdi), %%rdi\n";
  fprintf aout "\tmovq 24(%%rsi), %%rsi\n";
  fprintf aout "\tandq $-16, %%rsp\n";
  fprintf aout "\tcall strcmp\n";
  fprintf aout "\tcmp $0, %%eax\n";
  fprintf aout "\tje eq_true\n";
  fprintf aout "\tjmp eq_false\n";

  (* return in %rax *)
  fprintf aout "\n\t.globl eq_true\n\teq_true:\n";
  fprintf aout "\tmovq $1, %%rax\n";
  fprintf aout "\tjmp eq_handler_end\n";
  fprintf aout "\n\t.globl eq_false\n\teq_false:\n";
  fprintf aout "\tmovq $0, %%rax\n";
  fprintf aout "\tjmp eq_handler_end\n";
  fprintf aout "\n\t.globl eq_handler_end\n";
  fprintf aout "\teq_handler_end:\n";
  fprintf aout "\tmovq %%rbp, %%rsp\n";
  fprintf aout "\tpopq %%rbp\n";
  fprintf aout "\tret\n";

  (* print out is_void *)
  fprintf aout "\n## is_void\n";
  fprintf aout ".globl is_void\nis_void:\n";
  fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";
  fprintf aout "\tpushq %%rdi\n";
  fprintf aout "\tcall Bool..new\n";
  fprintf aout "\tpopq %%rdi\n";
  fprintf aout "\tcmpq $0, %%rdi\n";
  fprintf aout "\tjne is_void_false\n";
  fprintf aout "\tmovq $1, 24(%%r13)\n";
  fprintf aout ".globl is_void_false\nis_void_false:\n";
  fprintf aout "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n";

  (* new self_type *)
  fprintf aout "\n## new SELF_TYPE\n";
  fprintf aout ".globl self_type_handler\nself_type_handler:\n";
  fprintf aout "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n";
  fprintf aout "\tmovq 0(%%rdi), %%r13\n";
  Hashtbl.iter
    (fun cname ctag ->
      fprintf aout "\tmovq $%d, %%r14\n" ctag;
      fprintf aout "\tcmpq %%r14, %%r13\n";
      fprintf aout "\tje self_type_%s\n" cname)
    class_tags;
  Hashtbl.iter
    (fun cname ctag ->
      fprintf aout ".globl self_type_%s\nself_type_%s:\n" cname cname;
      call_new aout cname;
      fprintf aout "\tjmp self_type_end\n")
    class_tags;
  fprintf aout ".globl self_type_end\nself_type_end:\n";
  fprintf aout "\tpopq %%rbp\n";
  fprintf aout "\tret\n";

  (* print out program start *)
  fprintf aout "\n## PROGRAM BEGINS HERE\n";
  fprintf aout ".globl start\n";
  fprintf aout "start:\n";
  fprintf aout ".globl main\n";
  fprintf aout ".type main, @function\n";
  fprintf aout "main:\n";
  fprintf aout "\tmovq $Main..new, %%r14\n";
  fprintf aout "\tpushq %%rbp\n";
  fprintf aout "\tcall *%%r14\n";
  fprintf aout "\tpushq %%rbp\n";
  fprintf aout "\tpushq %%r13\n";
  fprintf aout "\tmovq $Main.main, %%r14\n";
  fprintf aout "\tcall *%%r14\n";
  fprintf aout "\t## guarantee 16-byte alignment before call\n";
  fprintf aout "\tandq $-16, %%rsp\n";
  fprintf aout "\tmovl $0, %%edi\n";
  fprintf aout "\tcall exit\n"
