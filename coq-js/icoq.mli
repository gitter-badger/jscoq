(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2015     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* Coq Interface to be used by JavaScript Ocaml code. Parts based in
   the coq source code.

   By Emilio J. Gallego Arias, Mines ParisTech, Paris.
*)

(* Init options for coq *)
type async_flags = {
  enable_async : string option;
  async_full   : bool;
  deep_edits   : bool;
}

type coq_opts = {

  (* callback to handle async feedback *)
  fb_handler : Feedback.feedback -> unit;

  (* Initial LoadPath XXX: Use the coq_pkg record? *)
  iload_path   : (string list * string * bool) list;

  (* Libs to require prior to STM init *)
  require_libs : (string list * string * bool option) list;

  (* Whether to enable implicit in the stdlib *)
  implicit_std : bool;

  (* Async flags *)
  aopts        : async_flags;

  (* name of the top-level module *)
  top_name     : string;

  (* callback to load cma/cmo files *)
  ml_load    : string -> unit;
}

(** [init opts] Initialize the Coq engine *)
val coq_init : coq_opts -> Stateid.t

(** [version] returns miscellaneous version information *)
val version : string * string * string * string

(** [add_load_path qid path has_ml] associate a coq package namespace
    [qid] to a [path], register for ml searching *)
val add_load_path : string list -> string -> bool -> unit

(** [richpp_of_goals ()] returns a richpp representing the current goals  *)
val richpp_of_goals : unit -> Richpp.richpp

(** [set_debug t] enables/disables debug mode  *)
val set_debug : bool -> unit
