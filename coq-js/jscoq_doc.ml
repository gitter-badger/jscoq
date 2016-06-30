(************************************************************************)
(* Coq SerAPI Plugin                                                    *)
(* Copyright 2016 Emilio J. Gallego Arias, MINES ParisTech              *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)
(* LICENSE: GPLv3+                                                      *)
(************************************************************************)

(******************************************************************************)
(* Taken from Core_kernel, (c) Jane Street, releaser under Apache License 2.0 *)
let split_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | t -> (List.rev acc, t)
  in
  loop [] xs

(* XXX: move to coq_pp_utils *)
module Pp = struct
  open Format
let pp_stateid fmt id = fprintf fmt "%d" (Stateid.to_int id)

let pp_str fmt str = fprintf fmt "%s" str

let pp_opt pp fmt opt = match opt with
  | None   -> ()
  | Some x -> fprintf fmt "%a" pp x

let rec pp_list ?sep pp fmt l = match l with
    []         -> fprintf fmt ""
  | csx :: []  -> fprintf fmt "@[%a@]" pp csx
  | csx :: csl -> fprintf fmt "@[%a@]%a@;%a" pp csx (pp_opt pp_str) sep (pp_list ?sep pp) csl
end
open Pp

(* Internal Coq document model: At the suggestion of Clément Pit--Claudel,
 * we extend the STM document model to work on a cancel-based fashion.
 *
 * All Coq STM edit/add operations *must* be accessed performed using
 * this interface in order to maintain consistency.
 *)

(* Our document model is linear for now *)
type ser_doc = Stateid.t list

let pp fmt l =
  Format.fprintf fmt "@[%a@]" (pp_list ~sep:" " pp_stateid) l

let _dump_doc doc =
  Format.eprintf "%a@\n%!" pp doc

let create () = [Stateid.initial]

(* Sadly this is not properly exported from Stm/Vernac *)
exception End_of_input
exception NoSuchState of Stateid.t

let parse_sentence = Flags.with_option Flags.we_are_parsing
  (fun pa ->
    match Pcoq.Gram.entry_parse Pcoq.main_entry pa with
    | Some (loc, _ast) -> loc
    | None             -> raise End_of_input
  )

(* Main add logic; we check that the ontop state is present in the
 * document, as it could well happen that the user request to add
 * arrives out of order wrt to a cancel request demanded by Coq, even
 * if I think we agree this shouldn't be possible. Then, we add and
 * update our document.
 *)
let add ?newtip ~doc ~ontop verb eid stm =
  if not (List.mem ontop doc) then raise (NoSuchState ontop);
  let pa = Pcoq.Gram.parsable (Stream.of_string stm)     in
  let loc         = parse_sentence pa                    in
  let new_st, foc = Stm.add ~ontop ?newtip verb eid stm  in
  let new_doc     = new_st :: doc                        in
  new_st, loc, foc, new_doc

(* invalid range returns a list of all the invalid stateid from
   can_st and the new doc _in reverse order_ *)
let invalid_range ~doc can_st =
  if not (List.mem can_st doc)
  then [], doc
  else split_while doc
        ~f:(fun st -> Stateid.newer_than st can_st || Stateid.equal st can_st)

(* XXX: Not implemented yet *)
let cancel_interval st (foc : Stm.focus) =
  let fmt = Format.err_formatter in
  Format.fprintf fmt "Cancel interval: [%a -- %a]" pp_stateid st pp_stateid Stm.(foc.stop);
  []

(* We follow a suggestion by Clément to report sentence invalidation
 * in a more modular way: When we issue the cancel command, we will
 * look for the cancelled part
 *)
let cancel ~doc can_st =
  (* dump_doc (); *)
  (* cancel and keep range *)
  let c_ran, k_ran = invalid_range ~doc can_st in
  (* Special case for a cancel on the initial state! *)
  let k_ran, edit_st = match k_ran with
    | []         -> [Stateid.initial], Stateid.initial
    | (st::rstm) -> (st::rstm), st
  in
  match Stm.edit_at edit_st with
  | `NewTip -> c_ran, k_ran
    (* - [tip] is the new document tip.
       - [st]   -- [stop] is dropped.
       - [stop] -- [tip]  has been kept.
       - [start] is where the editing zone starts
       - [add] happen on top of [id].
    *)
    | `Focus foc -> cancel_interval edit_st foc, doc


(* Edit is deprecated, we implement it in terms of cancel. *)
let edit ~doc edit_st =
  if not (List.mem edit_st doc)
  then [], doc
  else
    match Stm.edit_at edit_st with
    | `NewTip    -> split_while doc ~f:(fun st -> Stateid.newer_than st edit_st)
    | `Focus foc -> cancel_interval edit_st foc, doc

let observe ~doc sid =
  let _ = doc in
  Stm.observe sid
