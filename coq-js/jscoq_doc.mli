(************************************************************************)
(* Coq SerAPI Plugin                                                    *)
(* Copyright 2016 Emilio J. Gallego Arias, MINES ParisTech              *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)
(* LICENSE: GPLv3+                                                      *)
(************************************************************************)

type ser_doc = Stateid.t list

val create : unit -> ser_doc

val add :
  ?newtip:Stateid.t  ->
  doc:ser_doc        ->
  ontop:Stateid.t    ->
  bool               ->
  Feedback.edit_id   ->
  string             ->
  Stateid.t * Loc.t * [ `NewTip | `Unfocus of Stateid.t ] * ser_doc

val cancel  : doc:ser_doc -> Stateid.t -> Stateid.t list * ser_doc

(** [observe ~doc sid] evals up to state [sid] on document [doc]. *)
val observe : doc:ser_doc -> Stateid.t -> unit

(* Deprecated *)
val edit   : doc:ser_doc -> Stateid.t -> Stateid.t list * ser_doc

