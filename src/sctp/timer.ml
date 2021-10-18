let ( * ) = Int64.mul
let ( / ) = Int64.div
let ( + ) = Int64.add 
let ( - ) = Int64.sub

let min a b = if Int64.compare a b < 0 then a else b
let max a b = if Int64.compare a b >= 0 then a else b

(* This code is very much inspired by mirage/tcpip
   which follows a very similar algorithm:
  https://github.com/mirage/mirage-tcpip/blob/main/src/tcp/window.ml *)

module Rtx = struct 
  type t = {
    mutable srtt : int64; (* Smoothed round-trip time *)
    mutable rttvar : int64; (* Round-trip time variation *)
    mutable rto : int64; (* Retransmission timeout *)
    mutable first : bool; (* Has this first measurement been made *)
    rto_max : int64;
    rto_min : int64;

    mutable rtt_start_time : int64;
  }

  let v ?(rto_max = 60L * 1000L) () = {
    srtt = 0L;
    rttvar = 0L;
    rto = 3L * 1000L;
    first = false;
    rto_max = (assert (Int64.compare rto_max (60L * 1000L) >= 0); rto_max);
    rto_min = 1000L;
    rtt_start_time = 0L;
  }

  let beta v = v / 4L

  let one_minus_beta v = 3L * v / 4L

  let alpha v = v / 8L 
  let one_minus_alpha v = 7L * v / 8L


  module Make (C : Mirage_clock.MCLOCK) = struct 
    let update t = 
      t.rtt_start_time <- C.elapsed_ns ()

    (* 6.3.1 C2 & C3 *)
    let set t =
      (* Roun-trip time measurement *)
      let rtt = Int64.sub (C.elapsed_ns ()) t.rtt_start_time in
      (* If we've already taken the first measurement *)
      if t.first then begin
        t.rttvar <- one_minus_beta t.rttvar + beta (Int64.abs (t.srtt - rtt));
        t.srtt <- one_minus_alpha t.srtt + alpha rtt;
        t.rto <- t.srtt + 4L * t.rttvar
      end else begin
        t.srtt <- rtt;
        t.rttvar <- rtt / 2L;
        t.rto <- t.srtt + 4L * t.rttvar;
        t.first <- true 
      end;
      t.rto <- min (max t.rto t.rto_min) t.rto_max
    end
end

module Ack = struct 
  type t 
end