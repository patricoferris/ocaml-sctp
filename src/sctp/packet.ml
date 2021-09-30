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
    Fmt.pf ppf "src_port=%i dst_port=%i verification_tag=%ld checksum=%ld"
      src_port dst_port verification_tag checksum
end

type t = { header : Common_header.t; chunks : Chunk.t list }

let to_cstruct { header; chunks } =
  let ( <+> ) = Cstruct.append in
  let header = Common_header.to_cstruct header in
  let chunks =
    List.fold_left
      (fun buff c -> buff <+> Chunk.to_cstruct c)
      Cstruct.empty chunks
  in
  header <+> chunks

let of_cstruct buff =
  let length = Cstruct.length buff in
  let header = Common_header.of_cstruct buff in
  (* No chunks just header *)
  if length = 12 then Ok { header; chunks = [] }
    (* Common header + chunk header more than buff... Yikes! *)
  else if 16 > length then Error (`Msg "Not enough data in buff")
  else
    let offset = ref 12 in
    let chunks = ref [] in
    try
      while !offset < length do
        let b = Cstruct.sub buff !offset (length - !offset) in
        let chunk = Chunk.of_cstruct b in
        let chunk_length = Chunk.length ~with_padding:true chunk in
        chunks := chunk :: !chunks;
        offset := !offset + chunk_length
      done;
      Ok { header; chunks = List.rev !chunks }
    with Failure m | Invalid_argument m -> Error (`Msg m)

let pp ppf { header; chunks } =
  Fmt.pf ppf "header={%a} chunks=[%a]" Common_header.pp header
    (Fmt.list Chunk.pp) chunks

let equal a b =
  Common_header.equal a.header b.header
  && List.for_all2 Chunk.equal a.chunks b.chunks
