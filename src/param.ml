open Import

module type Type = sig
  type t

  val int_to_t : int -> t option

  val t_to_int : t -> int
end

module type S = sig
  type t

  val length : t -> int

  val of_cstruct : Cstruct.t -> (t, [ `Msg of string ]) result

  val to_cstruct : t -> Cstruct.t

  val fill : Cstruct.t -> int -> t -> unit
end

module Make (T : Type) = struct
  type t = { type_ : T.t; value : Cstruct.t }

  let length { value; _ } = Cstruct.length value + 4

  let of_cstruct buff =
    let type_ = T.int_to_t @@ Cstruct.BE.get_uint16 buff 0 in
    let length = Cstruct.BE.get_uint16 buff 2 in
    let value = Cstruct.sub buff 4 (length - 4) in
    match type_ with
    | Some type_ -> Ok { type_; value }
    | None -> Error (`Msg "Unknown parameter type")

  let to_cstruct ({ type_; value } as t) =
    let len = length t in
    let padding = pad len in
    let buff = Cstruct.create (len + padding) in
    Cstruct.BE.set_uint16 buff 0 (T.t_to_int type_);
    Cstruct.BE.set_uint16 buff 2 len;
    Cstruct.blit value 0 buff 4 (Cstruct.length value);
    buff

  let fill buff off ({ type_; value } as t) =
    let len = length t in
    Cstruct.BE.set_uint16 buff (off + 0) (T.t_to_int type_);
    Cstruct.BE.set_uint16 buff (off + 2) len;
    Cstruct.blit value 0 buff (off + 4) (Cstruct.length value);
    buff
end
