open Import

[%%cenum
type typ =
  | DATA [@id 0]
  | INIT [@id 1]
  | INIT_ACK [@id 2]
  | SACK [@id 3]
  | HEARTBEAT [@id 4]
  | HEARTBEAT_ACL [@id 5]
  | ABORT [@id 6]
  | SHUTDOWN [@id 7]
  | SHUTDOWN_ACK [@id 8]
  | ERROR [@id 9]
  | COOKIE_ECHO [@id 10]
  | COOKIE_ACK [@id 11]
  | ECNE [@id 12]
  | CWR [@id 13]
  | SHUTDOWN_COMPLETE [@id 14]
[@@uint8]]

(* The action to be taken upon receiving a
   chunk type that we don't understand *)
type action = Stop | Stop_report | Skip | Skip_report

let bool_to_int b = if b then 1 else 0

let typ_to_action t =
  match typ_to_int t asr 6 with
  | 0 -> Stop
  | 1 -> Stop_report
  | 2 -> Skip
  | 3 -> Skip_report
  | x -> failwith ("Unexpected chunk type higher 2 bits: " ^ string_of_int x)

type t = {
  type_ : [ `Type of typ | `Other of int ];
  flags : Cstruct.uint8;
  payload : Cstruct.t;
}

let type_to_int t = match t with `Type typ -> typ_to_int typ | `Other i -> i

let to_cstruct { type_; flags; payload } =
  (* 4 byte header + payload + padding *)
  let real_length = 4 + Cstruct.length payload in
  let padding = pad real_length in
  let length = real_length + padding in
  (* Buff is now padded appropriately *)
  let buff = Cstruct.create length in
  let i = type_to_int type_ in
  Cstruct.set_uint8 buff 0 i;
  Cstruct.set_uint8 buff 1 flags;
  Cstruct.BE.set_uint16 buff 2 real_length;
  (* What's the most efficient way to do this :| ? *)
  Cstruct.blit payload 0 buff 4 (real_length - 4);
  buff

let length ?(with_padding = false) t =
  let len = 4 + Cstruct.length t.payload in
  if with_padding then len + pad len else len

let of_cstruct buff =
  let typ = Cstruct.get_uint8 buff 0 in
  let type_ =
    match int_to_typ typ with Some i -> `Type i | None -> `Other typ
  in
  let flags = Cstruct.get_uint8 buff 1 in
  let len = Cstruct.BE.get_uint16 buff 2 in
  let payload = Cstruct.sub buff 4 (len - 4) in
  { type_; flags; payload }

let equal a b =
  a.type_ = b.type_ && a.flags = b.flags && Cstruct.equal a.payload b.payload

let pp ppf { type_; flags; payload } =
  Fmt.pf ppf "chunk: type=%i flags=%i payload=%a" (type_to_int type_) flags
    Cstruct.hexdump_pp payload

module Data = struct
  type t = {
    unordered : bool;
    beginning : bool;
    ending : bool;
    tsn : Cstruct.uint32;
    stream_id : Cstruct.uint16;
    seq_num : Cstruct.uint16;
    payload_protocol : Cstruct.uint32;
    payload : Cstruct.t;
  }

  type fragment = First | Last | Middle | Unfragmented

  let get_fragment_kind t =
    match (t.beginning, t.ending) with
    | true, false -> First
    | false, false -> Middle
    | false, true -> Last
    | true, true -> Unfragmented

  let of_chunk t =
    let unordered = t.flags land (1 lsl 2) > 0 in
    let beginning = t.flags land (1 lsl 1) > 0 in
    let ending = t.flags land 1 > 0 in
    let buff = t.payload in
    let tsn = Cstruct.BE.get_uint32 buff 4 in
    let stream_id = Cstruct.BE.get_uint16 buff 8 in
    let seq_num = Cstruct.BE.get_uint16 buff 10 in
    let payload_protocol = Cstruct.BE.get_uint32 buff 12 in
    let payload = Cstruct.sub buff 12 (Cstruct.length buff - 12) in
    {
      unordered;
      beginning;
      ending;
      tsn;
      stream_id;
      seq_num;
      payload_protocol;
      payload;
    }

  (* TODO: too much appending probably *)
  let to_chunk
      {
        unordered;
        beginning;
        ending;
        tsn;
        stream_id;
        seq_num;
        payload_protocol;
        payload;
      } =
    let type_ = `Type DATA in
    let flags =
      (bool_to_int unordered lsl 2)
      land (bool_to_int beginning lsl 1)
      land bool_to_int ending
    in
    let buff = Cstruct.create 12 in
    Cstruct.BE.set_uint32 buff 0 tsn;
    Cstruct.BE.set_uint16 buff 4 stream_id;
    Cstruct.BE.set_uint16 buff 6 seq_num;
    Cstruct.BE.set_uint32 buff 8 payload_protocol;
    { type_; flags; payload = Cstruct.append buff payload }

  let pp ppf
      {
        unordered;
        beginning;
        ending;
        tsn;
        stream_id;
        seq_num;
        payload_protocol;
        payload;
      } =
    Fmt.pf ppf
      "unordered=%b beginning=%b ending=%b tsn=%ld stream_id=%i seq_num=%i \
       payload_protocol=%ld payload=%a"
      unordered beginning ending tsn stream_id seq_num payload_protocol
      Cstruct.hexdump_pp payload
end

module Init_common (P : Param.S) = struct
  type t = {
    init_tag : Cstruct.uint32;
    a_rwnd : Cstruct.uint32;
    outbound : Cstruct.uint16;
    inbound : Cstruct.uint16;
    initial_tsn : Cstruct.uint32;
    params : P.t list;
  }

  let of_cstruct buff =
    let init_tag = Cstruct.BE.get_uint32 buff 0 in
    let a_rwnd = Cstruct.BE.get_uint32 buff 4 in
    let outbound = Cstruct.BE.get_uint16 buff 8 in
    let inbound = Cstruct.BE.get_uint16 buff 10 in
    let initial_tsn = Cstruct.BE.get_uint32 buff 12 in
    let remaining = Cstruct.length buff - 16 in
    (* There's probably a nice fold to do here... *)
    let off, params = (ref 16, ref []) in
    try
      while !off < remaining do
        let b = Cstruct.sub buff !off (remaining - !off) in
        let p =
          P.of_cstruct b |> function Ok v -> v | Error (`Msg m) -> failwith m
        in
        params := p :: !params;
        off := !off + P.length p
      done;
      Ok { init_tag; a_rwnd; outbound; inbound; initial_tsn; params = !params }
    with Failure m ->
      Error
        (`Msg
          (Fmt.str "Common init failed %s (off: %i, rem: %i)" m !off remaining))

  let to_cstruct { init_tag; a_rwnd; outbound; inbound; initial_tsn; params } =
    let p_length =
      List.fold_left
        (fun acc p ->
          let l = P.length p + acc in
          l + pad l)
        0 params
    in
    let buff = Cstruct.create (16 + p_length) in
    Cstruct.BE.set_uint32 buff 0 init_tag;
    Cstruct.BE.set_uint32 buff 4 a_rwnd;
    Cstruct.BE.set_uint16 buff 8 outbound;
    Cstruct.BE.set_uint16 buff 10 inbound;
    Cstruct.BE.set_uint32 buff 12 initial_tsn;
    let off = ref 16 in
    List.iter
      (fun p ->
        let l = P.length p in
        P.fill buff !off p;
        off := !off + (l + pad l))
      params;
    buff

  let fill buff off { init_tag; a_rwnd; outbound; inbound; initial_tsn; params }
      =
    (* Should probably check off + 12 <= len buff *)
    Cstruct.BE.set_uint32 buff (off + 0) init_tag;
    Cstruct.BE.set_uint32 buff (off + 4) a_rwnd;
    Cstruct.BE.set_uint16 buff (off + 8) outbound;
    Cstruct.BE.set_uint16 buff (off + 10) inbound;
    Cstruct.BE.set_uint32 buff (off + 12) initial_tsn;
    let off = ref (off + 16) in
    List.iter
      (fun p ->
        let l = P.length p in
        P.fill buff !off p;
        off := !off + (l + pad l))
      params;
    buff
end

module Init = struct
  module T = struct
    [%%cenum
    type t =
      | IPV4 [@id 5]
      | IPV6 [@id 6]
      | COOKIE_PRESERVATIVE [@id 9]
      | HOSTNAME [@id 11]
      | SUPPORTED_ADDRESS [@id 12]
    [@@uint16]]
  end

  module Params = Param.Make (T)
end
