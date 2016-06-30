(* Coq JavaScript API. Based in the coq source code and js_of_ocaml.
 *
 * By Emilio J. Gallego Arias, Mines ParisTech, Paris.
 * LICENSE: GPLv3+
 *
 * We provide a message-based asynchronous API for communication with
 * Coq. Our object listens to the following messages:
 *
 * And emits:
 *
 * - CoqLogEvent(level, msg): Log [msg] of priority [level].
 *
 *)

module Xml_datatype = struct

  type 'a gxml =
    [%import: 'a Xml_datatype.gxml
    ]
  [@@deriving yojson]

  type xml =
    [%import: Xml_datatype.xml
    [@with
      Xml_datatype.gxml := gxml;
    ]]
    [@@deriving yojson]

end

module Richpp = struct

  open Xml_datatype

  include Richpp

  let richpp_of_yojson sexp =
    let open Result in
    match xml_of_yojson sexp with
    | Ok xml  -> Ok (Richpp.richpp_of_xml xml)
    | Error s -> Error s

  let richpp_to_yojson rpp  = xml_to_yojson (Richpp.repr rpp)

end

module Loc = struct

  include Loc

  type _t =
    [%import: Loc.t]
    [@@deriving yojson]
    [@@warning -39]

  let of_yojson = _t_of_yojson
  let to_yojson = _t_to_yojson

end

module Stateid = struct

  include Stateid

  type _stateid = int
  [@@deriving yojson]
  [@@warning -39]

  let of_yojson json =
    let open Result in
    match _stateid_of_yojson json with
    | Ok id   -> Ok (Stateid.of_int id)
    | Error s -> Error s

  let to_yojson sid  = _stateid_to_yojson (Stateid.to_int sid)

end

type state_id = Stateid.t
  [@@deriving yojson]

type level =
  [%import: Feedback.level]
  [@@deriving yojson]
  [@@warning -39]

type edit_id =
  [%import: Feedback.edit_id]
  [@@deriving yojson]

type route_id =
  [%import: Feedback.route_id]
  [@@deriving yojson]

type edit_or_state_id =
  [%import: Feedback.edit_or_state_id]
  [@@deriving yojson]

type feedback_content =
  [%import: Feedback.feedback_content]
  [@@deriving yojson]

type feedback =
  [%import: Feedback.feedback
  [@with
     Feedback.route_id := route_id;
  ]]
  [@@deriving yojson]
