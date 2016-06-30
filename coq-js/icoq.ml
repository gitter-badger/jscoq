(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(************************************************************************)
(* Coq serialization API/Plugin                                         *)
(* Copyright 2016 MINES ParisTech                                       *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)

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


(**************************************************************************)
(* Low-level, internal Coq initialization                                 *)
(**************************************************************************)
let coq_init opts =
  let open Names in

  (* Custom toplevel is used for bytecode-to-js dynlink  *)
  let ser_mltop : Mltop.toplevel = let open Mltop in
    {
    load_obj = opts.ml_load;
    (* We ignore all the other operations for now. *)
    use_file = (fun _ -> ());
    add_dir  = (fun _ -> ());
    ml_loop  = (fun _ -> ());
  } in

  Mltop.set_top ser_mltop;

  (* Coq library initialization *)
  Lib.init();

  (* Mltop.init_known_plugins (); *)

  Goptions.set_string_option_value ["Default";"Proof";"Mode"] "Classic";

  Global.set_engagement Declarations.PredicativeSet;

  (**************************************************************************)
  (* Library initialization                                                 *)
  (**************************************************************************)
  Loadpath.add_load_path "." Nameops.default_root_prefix ~implicit:false;

  List.iter (fun (lib, lib_path, has_ml) ->
      let open Names in
      let coq_path = DirPath.make @@ List.rev @@ List.map Id.of_string lib in
      Loadpath.add_load_path lib_path coq_path ~implicit:opts.implicit_std;
      if has_ml then Mltop.add_ml_dir lib_path
    ) opts.iload_path;

  (* We need to declare a toplevel module name.
   *
   * Not sure if this restriction can be removed
   *)
  let sertop_dp = DirPath.make [Id.of_string opts.top_name] in
  Declaremods.start_library sertop_dp;

  (**************************************************************************)
  (* Load the prelude                                                       *)
  (**************************************************************************)

  (* We must prevent output to stdout when loading the libs... but it
   * will be lost.
   *)
  Feedback.set_logger Feedback.feedback_logger;

  List.iter (fun (dpl, p, in_exp) ->
      let dp = DirPath.make @@ List.rev_map Id.of_string dpl in
      Library.require_library_from_dirpath [dp,p] in_exp
    ) opts.require_libs;

  (* Note that we don't emit feedback during this phase _by design_.        *)

  (**************************************************************************)
  (* Feedback setup                                                         *)
  (**************************************************************************)

  (* Whether to forward Glob output to the IDE. *)
  let dumpglob = false in
  let dump_opt =
    if dumpglob then begin
      Dumpglob.feedback_glob ();
      "-feedback-glob"
    end else begin
      Dumpglob.noglob ();
      "-no-glob"
    end
  in

  (* Initialize logging. *)
  Feedback.add_feeder opts.fb_handler;

  (**************************************************************************)
  (* Async setup                                                            *)
  (**************************************************************************)

  (* Set async flags; IMPORTANT, this has to happen before STM.init () ! *)
  Option.iter (fun coqtop ->

      Flags.async_proofs_mode := Flags.APon;
      (* Imitate CoqIDE *)
      Flags.async_proofs_full := opts.aopts.async_full;
      Flags.async_proofs_never_reopen_branch := not opts.aopts.deep_edits;
      Flags.async_proofs_flags_for_workers := [dump_opt];
      Flags.async_proofs_n_workers    := 3;
      Flags.async_proofs_n_tacworkers := 3;
      (* async_proofs_worker_priority); *)
      CoqworkmgrApi.(init Flags.High);
      (* Uh! XXXX *)
      for i = 0 to Array.length Sys.argv - 1 do
        Array.set Sys.argv i "-m"
      done;
      Array.set Sys.argv 0 coqtop
    ) opts.aopts.enable_async;

  (**************************************************************************)
  (* Start the STM!!                                                        *)
  (**************************************************************************)
  Stm.init();

  (* Miscellaneous tweaks *)
  Vernacentries.enable_goal_printing := false;
  Vernacentries.qed_display_script   := false;

  (* Flags.debug := true; *)

  (* Return the initial state of the STM *)
  Stm.get_current_state ()

(* Add a load path *)
let add_load_path pkg pkg_path has_ml =
  let open Names                                                       in
  let coq_path = DirPath.make @@ List.rev @@ List.map Id.of_string pkg in
  Loadpath.add_load_path ("./" ^ pkg_path) coq_path ~implicit:false;
  if has_ml then Mltop.add_ml_dir pkg_path

(* XXX: We want to implement our custom proof printer (from
 * printing/printer.ml
 *
 * At a minimum, we want to output a the list of hypothesis and the
 * goal separatedly. See pr_context_of.
 *)
let richpp_of_goals () =
  let pp_goals =
    try Printer.pr_open_subgoals ()
    with Proof_global.NoCurrentProof -> Pp.str ""
  in
  Richpp.richpp_of_pp pp_goals

(** [set_debug t] enables/disables debug mode  *)
let set_debug debug =
  Backtrace.record_backtrace debug;
  Flags.debug := debug

let version =
  Coq_config.version, Coq_config.date, Coq_config.compile_date, Coq_config.caml_version

(*
let print_toplevel_error (e, info) =
  let any = Errors.push any in
  let msg = print_toplevel_error any ++ fnl () in
  Errors.iprint (e, info)

let pr_open_cur_subgoals () =
  try Printer.pr_open_subgoals ()
  with Proof_global.NoCurrentProof -> str ""

*)
