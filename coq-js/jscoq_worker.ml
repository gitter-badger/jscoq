(* Coq JavaScript API.
 *
 * (c) Emilio J. Gallego Arias, Mines ParisTech, Paris.
 * LICENSE: GPLv3+
 *
 * We provide a message-based asynchronous API for communication with
 * Coq.
 *
 *)

open Jser_feedback

let rstr = Richpp.richpp_of_string
let rpp  = Richpp.richpp_of_pp

type gvalue =
  [%import: Goptions.option_value]
  [@@deriving yojson]

(* Main RPC calls *)
type jscoq_cmd =
  | GetInfo
  | InfoPkg of string * string list
  | LoadPkg of string * string
  (*           initial_imports      load paths              *)
  | Init    of string list list   * string list list

  | Add     of Stateid.t * int * string
  | EditAt  of Stateid.t
  | Cancel  of Stateid.t
  | Observe of Stateid.t
  | Goals
  | SetOpt  of bool option * string list * gvalue
  | GetOpt  of string list
  [@@deriving yojson]

type jscoq_answer =
  (* XXX: Init? *)
  | Added     of Stateid.t
  | Cancelled of Stateid.t list
  | GoalInfo  of Stateid.t * Richpp.richpp
  | CoqInfo   of string
  | Log       of level     * Richpp.richpp
  | CoqOpt    of gvalue
  | Feedback  of feedback
  | CoqExn    of Loc.t option * (Stateid.t * Stateid.t) option * Richpp.richpp
  | JsonExn   of string
  [@@deriving yojson]

let rec json_to_obj (cobj : < .. > Js.t) (json : Yojson.Safe.json) : < .. > Js.t =
  let open Js.Unsafe in
  let ofresh j = json_to_obj (obj [||]) j in
  match json with
  | `Bool b   -> coerce @@ Js.bool b
  | `Null     -> pure_js_expr "undefined"
  | `Assoc l  -> List.iter (fun (p, js) -> set cobj p (ofresh js)) l; cobj
  | `List  l  -> Array.(Js.array @@ map ofresh (of_list l))
  | `Float f  -> coerce @@ Js.number_of_float f
  | `String s -> coerce @@ Js.string s
  | `Int m    -> coerce @@ Js.number_of_float (Obj.magic m)
  | `Intlit s -> coerce @@ Js.number_of_float (float_of_string s)
  | `Tuple t  -> Array.(Js.array @@ map ofresh (of_list t))
  | `Variant(_,_) -> pure_js_expr "undefined"

let _answer_to_jsobj msg =
  let json_msg = jscoq_answer_to_yojson msg                            in
  let json_str = Yojson.Safe.to_string json_msg                        in
  (* Workaround to avoid ml_string conversion of Json.unsafe_input     *)
  Js.Unsafe.global##.JSON##(parse (Js.string json_str))

let answer_to_jsobj msg =
  let json_msg = jscoq_answer_to_yojson msg                            in
  json_to_obj (Js.Unsafe.obj [||]) json_msg

type progress_info =
  [%import: Jslibmng.progress_info]
  [@@deriving yojson]

type lib_event =
  [%import: Jslibmng.lib_event]
  [@@deriving yojson]

let _lib_event_to_jsobj msg =
  let json_msg = lib_event_to_yojson msg                               in
  let json_str = Yojson.Safe.to_string json_msg                        in
  (* Workaround to avoid ml_string conversion of Json.unsafe_input     *)
  Js.Unsafe.global##.JSON##(parse (Js.string json_str))

let lib_event_to_jsobj msg =
  let json_msg = lib_event_to_yojson msg                            in
  json_to_obj (Js.Unsafe.obj [||]) json_msg

(* Send messages to the main thread *)
let post_answer (msg : jscoq_answer) : unit =
  Worker.post_message (answer_to_jsobj msg)

let post_lib_event (msg : lib_event) : unit =
  Worker.post_message (lib_event_to_jsobj msg)

let exec_setopt loc n (v : Goptions.option_value) =
  let open Goptions in
  match v with
  | BoolValue b      -> set_bool_option_value_gen loc n b
  | IntValue  i      -> set_int_option_value_gen  loc n i
  | StringValue s    -> set_string_option_value_gen loc n s
  | StringOptValue s -> set_string_option_value_gen loc n (Option.default "" s)

(* lib_init  : list of modules to load *)
(* lib_paths : list of paths *)
let exec_init (lib_init : string list list) (lib_path : string list list) =
  (* XXX: to_dir *)
  (* let to_name   = String.concat "."  in *)
  let to_dir    = String.concat "/"  in

  (* require_libs : (string list * string * bool option) list; *)
  (*                 module        file     ??                 *)
  (* example         [Coq,Init,Prelude] "Coq/Init/Prelude.vo", Some false *)
  let lib_require  = List.map (fun lp ->
      (* Format.eprintf "u: %s, %s@\n" (to_name md) (to_dir md); *)
      lp, to_dir lp ^ ".vo", Some false) lib_init  in
  (* None       : just require            *)
  (* Some false : import but don't export *)
  (* Some true  : import and export       *)

  (* iload_path   : (string list * string * bool) list; *)
  let lib_load_path = List.map (fun md ->
      (* Format.eprintf "u: %s, %s@\n" (to_name md) (to_dir md); *)
      md, to_dir md, true)               lib_path in

  Icoq.(coq_init {
      ml_load    = Jslibmng.coq_cma_link;
      fb_handler = (fun fb -> post_answer (Feedback fb));
      require_libs = lib_require;
      iload_path   = lib_load_path;
      implicit_std = false;
      top_name     = "JsCoq";
      aopts        = { enable_async = None;
                       async_full   = false;
                       deep_edits   = false;
                     }
    })

(* I refuse to comment on this part of Coq code... *)
let exec_getopt on =
  let open Goptions in
  let tbl = get_tables () in
  (OptionMap.find on tbl).opt_value

let jscoq_execute =
  let out_fn = post_answer in fun doc -> function
  | Add (sid,eid,stm) -> let nsid,_loc,_,ndoc = Jscoq_doc.add ~doc:!doc ~ontop:sid false eid stm in
                         doc := ndoc; out_fn @@ Added (nsid)

  | Cancel  sid       -> let can_st, ndoc = Jscoq_doc.cancel ~doc:!doc sid in
                         doc := ndoc; out_fn @@ Cancelled can_st

  | EditAt  sid       -> let can_st, ndoc = Jscoq_doc.edit   ~doc:!doc sid in
                         doc := ndoc; out_fn @@ Cancelled can_st

  | Observe sid       -> Jscoq_doc.observe ~doc:!doc sid;
                         out_fn @@ Log (Debug, rstr @@ "observe " ^ (Stateid.to_string sid))

  | Goals             -> out_fn @@ GoalInfo (Stm.get_current_state (), Icoq.richpp_of_goals ())

  | SetOpt (l, on, ov)  -> exec_setopt l on ov
  | GetOpt on           -> out_fn @@ CoqOpt (exec_getopt on)

  | Init(lib_init, lib_path) -> let iid = exec_init lib_init lib_path in
                                out_fn @@ Log (Debug, rstr @@ "init " ^ (Stateid.to_string iid))

  | InfoPkg(base, pkgs) -> Lwt.async (fun () -> Jslibmng.info_pkg post_lib_event base pkgs)

  (* XXX: Must add the libs *)
  | LoadPkg(base, pkg)  -> Lwt.async (fun () -> Jslibmng.load_pkg post_lib_event base pkg)

  | GetInfo             ->
    let coqv, coqd, ccd, ccv = Icoq.version                     in
    let header1 = Printf.sprintf
        " JsCoq beta, Coq %s (%s),\n   compiled on %s\n Ocaml %s"
        coqv coqd ccd ccv                                       in
    let header2 = Printf.sprintf
        " Js_of_ocaml version %s\n" Sys_js.js_of_ocaml_version  in
    out_fn @@ CoqInfo (header1 ^ header2)

let setup_pseudo_fs () =
  Sys_js.register_autoload ~path:"/" (fun (_,s) -> Jslibmng.coq_vo_req s)

let setup_std_printers () =
  Sys_js.set_channel_flusher stdout (fun msg -> post_answer (Log (Notice, rstr @@ "stdout: " ^ msg)));
  Sys_js.set_channel_flusher stderr (fun msg -> post_answer (Log (Notice, rstr @@ "stderr: " ^ msg)));
  ()

let coq_exn_info exn =
    let (e, info) = CErrors.push exn                in
    let pp_exn    = rpp @@ CErrors.iprint (e, info) in
    CoqExn (Loc.get_loc info, Stateid.get info, pp_exn)

let jscoq_protect f =
  try f ()
  with | exn -> post_answer @@ coq_exn_info exn

(* Message from the main thread *)
let on_msg doc obj =
  (* XXX: Cal the GC, setTimeout to avoid stack overflows ?? *)
  let json_obj = Yojson.Safe.from_string (Js.to_string (Json.output obj)) in

  match jscoq_cmd_of_yojson json_obj with
  | Result.Ok cmd  -> jscoq_protect (fun () -> jscoq_execute doc cmd)
  | Result.Error s -> post_answer @@
    JsonExn ("Error in JSON conv: " ^ s ^ " | " ^ (Js.to_string (Json.output obj)))

(* This code is executed on Worker initialization *)
let _ =

  (* This is needed if dynlink is enabled in 4.03.0 *)
  Sys.interactive := false;

  setup_pseudo_fs    ();
  setup_std_printers ();

  let doc = ref (Jscoq_doc.create ()) in

  (* Heuristic to avoid StackOverflows when we have too many incoming
     messages. *)
  (*
  let on_msg obj = Lwt.(async (             fun () ->
                        Lwt_js.yield () >>= fun () ->
                        return @@ on_msg obj    )) in
   *)

  let on_msg = on_msg doc  in
  Worker.set_onmessage on_msg
