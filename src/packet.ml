module Common_header = struct
  type t = {
    src_port : Cstruct.uint16;
    dst_port : Cstruct.uint16;
    verification_tag : Cstruct.uint32;
    checksum : Cstruct.uint32;
  }

  let of_cstruct buff =
    let src_port = Cstruct.BE.get_uint16 buff 0 in
    let dst_port = Cstruct.BE.get_uint16 buff 2 in
    let verification_tag = Cstruct.BE.get_uint32 buff 4 in
    let checksum = Cstruct.BE.get_uint32 buff 8 in
    { src_port; dst_port; verification_tag; checksum }

  let to_cstruct { src_port; dst_port; verification_tag; checksum } =
    let buff = Cstruct.create 12 in
    Cstruct.BE.set_uint16 buff 0 src_port;
    Cstruct.BE.set_uint16 buff 2 dst_port;
    Cstruct.BE.set_uint32 buff 4 verification_tag;
    Cstruct.BE.set_uint32 buff 8 checksum;
    buff

  let equal a b = a = b

  let pp ppf { src_port; dst_port; verification_tag; checksum } =
    Fmt.pf ppf
      "header: src_port=%i dst_port=%i verification_tag=%ld checksum=%ld"
      src_port dst_port verification_tag checksum
end

type t = { header : Common_header.t; chunks : Chunk.t list }

let of_cstruct buff =
  let length = Cstruct.length buff in
  let header = Common_header.of_cstruct buff in
  (* No chunks just header *)
  if length = 12 then Ok { header; chunks = [] }
    (* Common header + chunk header more than buff... Yikes! *)
  else if 16 > length then Error (`Msg "Not enough data in buff")
  else Error (`Msg "TOOOODODODODODODODO")
