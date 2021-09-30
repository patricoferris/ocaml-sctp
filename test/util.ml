let list_to_cstruct xs =
  let open Cstruct in
  let buf = create (List.length xs) in
  List.iteri (set_uint8 buf) xs;
  buf

let msg : [ `Msg of string ] Alcotest.testable =
  Alcotest.of_pp (fun ppf (`Msg s) -> Fmt.pf ppf "msg(%s)" s)

let cstruct = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal
