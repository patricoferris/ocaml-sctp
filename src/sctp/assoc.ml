(* The Association

  Before any data can be sent between two endpoints, the
  initialisation process must first be completed. The normal
  set-up when "A" wishes to send to "Z" is: 
  
    1. "A" sends an INIT chunk to "Z" and enters the COOKIE-WAIT
       state. 
    2. "Z" responds immediately with INIT-ACK. "Z" should not allocate 
       any resources here otherwise they're vulnerable to attacks.
    3. "A" receives it and responds entering COOKIE-ECHOED
    4. "Z" responds with COOKIE-ACK after building a TCB and enters 
       ESTABLISHED.
    5. "A" recives ACK and enter ESTABLISHED *)

let src = Logs.Src.create "sctp-association" ~doc:"SCTP Association"
module Log = (val Logs.src_log src : Logs.LOG)
   
type state = 
  | Closed
  | Cookie_wait
  | Cookie_echoed
  | Established
  | Shutdown_pending
  | Shutdown_sent
  | Shutdown_recv
  | Shutdown_ack_sent

type t = {
   state : state;
}

let abort chunk f = match chunk.Chunk.type_ with
   | `Type Chunk.ABORT -> Closed
   | _ -> f ()


let next chunk t = match t.state with
   | Cookie_wait -> 
      Log.debug (fun f -> f "cookie-wait and recv: %a" Chunk.pp chunk);
      abort chunk (fun () -> Cookie_echoed)
   | Cookie_echoed -> abort chunk (fun () -> Established)
   | Closed -> (match chunk.Chunk.type_ with 
   | `Type Chunk.INIT -> 
      (* Cookie gen *)
      Closed
      | `Type COOKIE_ECHO -> Established
      | _ -> assert false)
   | Established -> (
      match chunk.Chunk.type_ with
      | `Type Chunk.SHUTDOWN -> Shutdown_recv 
      | _ -> assert false
   )
   | _ -> assert false

      