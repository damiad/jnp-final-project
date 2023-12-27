open ProtocolDefinitions
open Protocol
open String
open Bytes
open Uint63
open Unix
open List

(** The module with the potentially infinite protocol loop that handles
    low-level network communication. *)

(** The timeout of one second to wait for a packet to be received. *)
let timeout = 1.0

(** The constant representing the value representing
    `any local addresspoint'. *)
let nulladdr = ADDR_INET (inet_addr_of_string "0.0.0.0", 0)

(** The low-level function that handles data reception within
    the configured timeout. *)
let recvtimeout (ntwrk: Unix.file_descr) recv_packet num1 num2 =
  let (r1, _, _) = (** wait until data is ready or timeout passes *)
    select [ntwrk] [] [] timeout
  in
  if List.mem ntwrk r1 then
    let (sz, oth) = recvfrom ntwrk recv_packet num1 num2 [] in
    (sz, oth, RCVOK)
  else
    (0, nulladdr, RCVTIMEOUT)

(** It converts bytes to string but up to *)
let my_string_of_bytes recv_packet =
  let raw = Bytes.to_string recv_packet in
  let prts = String.split_on_char '\000' raw in
  match prts with
  | [] -> ""
  | hd :: tl -> hd

let rec do_client_step = fun
    next_step_op
    (recv_packet: bytes)
    (st_pci: coq_PCState * coq_CPInput)
    (saddr: sockaddr)
    (conn: file_descr) ->
  let (st, pci) = st_pci in
  let ressent =
    match pci with
    | IPacket (bootp_msg, plen) ->
      print_string "Sending out BOOTP packet:\n";
      print_string bootp_msg;
      print_newline ();
      sendto conn (Bytes.of_string bootp_msg) 0 (hash plen) [] saddr
    | Init (_, _, _, _, _, _) -> -1
    | PIError _ -> -1
  in
  if ressent < 0 then
    do_client_step next_step_op recv_packet
      (SError "Error in sending BOOTP packet to remote server",
       PIError "Error in sending BOOTP packet to remote server") saddr conn
  else
    let (sz, otherside, status) = recvtimeout conn recv_packet 0 516 in
    let (tm, plen) =
      match status with
      | RCVOK -> (false, of_int sz)
      | RCVTIMEOUT -> (true, of_int 0)
    in
    let rcvd = my_string_of_bytes recv_packet in
    let (nst, pout) =
      print_string "Received data: ";
      print_string rcvd;
      print_newline ();
      next_step_op st (IPacket (rcvd, plen))
    in
    match pout with
    | Nothing -> OK
    | OPacket content ->
      do_client_step next_step_op recv_packet
        (nst, IPacket (content, of_int (String.length content)))
        saddr conn

(** The function that handles the main protocol loop on the client side. *)
let client_loop
    next_step_op
    (init: coq_PCState * coq_CPInput)
    (conn: file_descr)
    (server_addr: inet_addr)
    (port: int) =
  print_string "Entered BOOTP client protocol loop\n";
  let portaddr = ADDR_INET (server_addr, port) in
  let buffer_size = 1024 in
  let recv_packet = Bytes.make buffer_size '\000' in
  do_client_step next_step_op recv_packet init portaddr conn

(** The low-level function that handles data reception. *)
let recv_srvr (ntwrk: file_descr list) recv_packet num1 num2 =
  let (r1, _, _) = (** wait until data is ready or timeout passes *)
    print_string "Waiting for incoming data";
    print_newline ();
    select ntwrk [] [] (-1.0) (* we wait indefinitely *)
  in
  match ntwrk with
  | [] -> (0, nulladdr)
  | hd :: tl ->
    if List.mem hd r1 then
      let (sz, oth) = recvfrom hd recv_packet num1 num2 [] in
      (sz, oth)
    else
      (0, nulladdr)

let print_addr addr =
  (match addr with
  | ADDR_UNIX s -> print_string ("Unix addr: " ^ s)
  | ADDR_INET (addr, n) ->
      print_string ("INET addr: " ^ string_of_inet_addr addr ^ ":" ^ string_of_int n))

let get_conns_from_st st =
  match st with
  | SNoOp (conn, addr) ->
      let _ =
        print_string "About to bind to address:port ";
        print_addr addr;
        print_newline ();
        bind conn addr
      in
      [conn]
  | SFComm ((((_, fds), _), c), _) -> c :: fds
  | _ -> []

(** The function that handles the main protocol loop on the server side. *)
let rec do_server_step = fun
    next_step_op
    (packet_buffer: bytes)
    (st_spi: coq_PSState * coq_SPInput) ->
  let (st, spi) = st_spi in
  let conns = get_conns_from_st st in
  let (* we wait to receive a packet *)
    (size, addr) = recv_srvr conns packet_buffer 0 516
  in
  print_addr addr;
  print_newline ();
  if size < 0 then
    (* after unsuccessful packet receive, we just repeat *)
    (print_string "Error in packet receive\n";
    do_server_step next_step_op packet_buffer st_spi)
  else
    (* after successful packet receive we do next step *)
    let strfbuf = Bytes.to_string packet_buffer in
    print_string "Received packet: ";
    print_string strfbuf;
    print_newline ();
    let (nst, pout) =
      next_step_op st (SIPacket (addr, Bytes.to_string packet_buffer))
    in
    match nst with
    | SNoOp _ ->
        (* Initial state is always outside the loop so
                      this is an error. *)
        RunError "Unexpected initial state in the middle of the BOOTP protocol"
    | SFinal ->
        (* We successfully entered the final state so the final
                    result is OK. This should not happen. *)
        OK
    | SSError reason ->
        (* Error means the final result is an error. *)
        RunError reason
    | SFComm (pdata, _) ->
        (* Further communication should take place. *)
        (* We try to send out data back to the client *)
        match conns with
        | [] -> RunError "No socket to send data through."
        | hd :: tl ->
            match pout with
            | Nothing ->
                (* In case nothing is to be sent, we just repeat communication
                 loop. *)
                let st_pi = (nst, SNothing) in
                do_server_step next_step_op packet_buffer st_pi
            | OPacket content ->
                let _ =
                  blit_string content 0 packet_buffer 0 (String.length content)
                in
                let (* We send data it out here. *)
                  status =
                    print_string "Sending out: ";
                    print_string content;
                    print_newline ();
                    sendto hd packet_buffer 0 (String.length content) [] addr
                in
                if status > 0 then
                  let st_pi = (nst, SNothing) in
                  do_server_step next_step_op packet_buffer st_pi
                else
                  RunError "Sending error"

(** The function that handles the main protocol loop. *)
let srvr_loop
    next_step_op
    (conn: file_descr)
    (server_addr: inet_addr)
    (port: int) =
  print_string "Entered BOOTP server protocol loop\n";
  let portaddr = ADDR_INET (server_addr, port) in
  let buffer_size = 1024 in
  let recv_packet = Bytes.make buffer_size '\000' in
  let st_spi = (SNoOp (conn, portaddr), SNothing) in
  do_server_step next_step_op recv_packet st_spi

