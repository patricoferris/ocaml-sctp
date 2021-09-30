open Util
open Sctp

(* Some tests borrowed from https://github.com/pion/sctp/blob/master/packet_test.go *)
let packet = Alcotest.testable Packet.pp Packet.equal

let test_parse buff expected () =
  let p = Packet.of_cstruct buff in
  Alcotest.(check (result packet msg)) "same packet" p expected

let test_encode_decode buff () =
  let p = Packet.of_cstruct buff |> Result.get_ok in
  let buff' = Packet.to_cstruct p in
  Alcotest.(check cstruct) "same buff" buff' buff

let packets =
  [
    ( "header_only",
      list_to_cstruct
        [
          0x13; 0x88; 0x13; 0x88; 0x00; 0x00; 0x00; 0x00; 0x06; 0xa9; 0x00; 0xe1;
        ],
      Ok
        Packet.
          {
            header =
              Common_header.
                {
                  src_port = 5000;
                  dst_port = 5000;
                  verification_tag = 0l;
                  checksum = 111739105l;
                };
            chunks = [];
          } );
  ]

let tests =
  let open Alcotest in
  let decoding =
    List.map
      (fun (s, buff, packet) ->
        test_case (Fmt.str "dec_%s" s) `Quick (test_parse buff packet))
      packets
  in
  let encdec =
    List.map
      (fun (s, buff, _) ->
        test_case (Fmt.str "encdec_%s" s) `Quick (test_encode_decode buff))
      packets
  in
  decoding @ encdec
