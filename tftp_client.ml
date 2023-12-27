open ProtocolDefinitions
open Protocol
open String
open Unix
open Protoloop
open Helper
open List

(** Get socket from the system socket library. *)
let get_socket_connection (host:string) (port:int) =
        socket PF_INET SOCK_DGRAM 0;;

(** The function initialises and runs the protocol procedure.
   + dir - direction of transmission (inbound, outbound)
   + host - the DNS address of the server to contact to
   + port - the port of the server to contact to
   + rfn - remote file name
   + lfn - local file name
   + mode - file transfer mode (i.e. ASCII or OCTET)
 *)
let start_tftp dir host port lfn rfn mode = 
  let conn = get_socket_connection host port in
  let _ = setsockopt conn SO_BROADCAST true in
  let he = gethostbyname host in
  let server_addr = he.h_addr_list.(0) in
  let saport = ADDR_INET (server_addr, port) in

  (* Using BOOTP to obtain connection details *)
  let bootp_msg = generate_bootp_request lfn in
  let initdir = Init (dir, conn, lfn, rfn, mode, saport) in
  let inito = proto_next NoOp initdir in

  match inito with
  | (st, OPacket pckt) ->
     let ipacket = IPacket (pckt, Uint63.of_int (String.length pckt)) in
     client_loop proto_next (st, ipacket) conn server_addr port
  | (SError msg, _) -> RunError msg
  | (_, _) -> RunError "Unexpected error in initialization"
;;


(** Parsing of the command-line arguments. *)
let check_args() =
  if Array.length Sys.argv <> 10
  then
    (false,Upload,"","","",0)
  else if Sys.argv.(2) <> "-l" || Sys.argv.(4) <> "-r" ||
            Sys.argv.(6) <> "-h" || Sys.argv.(8) <> "-p"
  then (false,Upload,"","","",0)
  else
    let mqdir = (if Sys.argv.(1) = "-d"
                then (Some Download)
                else if Sys.argv.(1) = "-u"
                then (Some Upload)
                else None) in
    match mqdir with
    | Some dir ->
       (try
          let port = int_of_string Sys.argv.(9) in
          (true,dir,Sys.argv.(3),Sys.argv.(5),Sys.argv.(7), port)
        with Failure _ ->
              (false,Upload,"","","",0))
    | None -> (false,Upload,"","","",0);;


let print_usage ()  =
  begin
    print_string "Usage: tftp [-d|-u] -l <local_file> -r <remote_file> -h <host> -p <port>";
    print_newline ();
    print_string "-d stands for download";
    print_newline ();
    print_string "-u stands for upload";
    print_newline ()
  end;;

let print_hello load lfn rfn host port =
  begin
    print_string("We ");
    print_string(dir_to_string load);
    print_string(" file ");
    print_string(lfn);
    print_string(" to ");
    print_string(rfn);
    print_string(" at the host ");
    print_string(host);
    print_string(":");
    print_int(port);
    print_newline ();
  end;;


(** The function main checks for correct command-line arguments and
   runs proper version of the client.  *)
let main () =
  let (ok,load, lfn, rfn, host, port) = check_args() in
  if  not ok
  then print_usage()
  else
    begin
      print_hello load lfn rfn host port;
      (* We can change the transfer mode at compile time. *)
      match (start_tftp load host port lfn rfn "binary") with
      | OK -> print_string "Done OK\n"
      | RunError strerr -> print_string ("Error: " ^ strerr ^ "\n")
    end
;;

(* Very simplistic start of the main function together with a test if
   the program runs in an interpreter.  *)
if !Sys.interactive then () else main ();;
