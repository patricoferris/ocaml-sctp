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
  val pp : t Fmt.t
  val equal : t -> t -> bool
end

module Make (T : Type) = struct
  type typ = [ `Type of T.t | `Unknown of int ]

  let typ_to_int = function `Type i -> T.t_to_int i | `Unknown i -> i

  let typ_of_int i =
    match T.int_to_t i with Some i -> `Type i | None -> `Unknown i

  type t = { type_ : typ; value : Cstruct.t }

  let pp ppf t =
    Fmt.pf ppf "type=%i value=%a" (typ_to_int t.type_) Cstruct.hexdump_pp
      t.value

  let equal a b = a.type_ = b.type_ && Cstruct.equal a.value b.value
  let length { value; _ } = Cstruct.length value + 4

  let of_cstruct buff =
    let i = Cstruct.BE.get_uint16 buff 0 in
    let type_ = typ_of_int i in
    let length = Cstruct.BE.get_uint16 buff 2 in
    let value = Cstruct.sub buff 4 (length - 4) in
    Ok { type_; value }

  let to_cstruct ({ type_; value } as t) =
    let len = length t in
    let padding = pad len in
    let buff = Cstruct.create (len + padding) in
    Cstruct.BE.set_uint16 buff 0 (typ_to_int type_);
    Cstruct.BE.set_uint16 buff 2 len;
    Cstruct.blit value 0 buff 4 (Cstruct.length value);
    buff

  let fill buff off ({ type_; value } as t) =
    let len = length t in
    Cstruct.BE.set_uint16 buff (off + 0) (typ_to_int type_);
    Cstruct.BE.set_uint16 buff (off + 2) len;
    Cstruct.blit value 0 buff (off + 4) (Cstruct.length value)
end
