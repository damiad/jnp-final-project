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
   + host - the DNS address of the server to contact
   + port - the port of the server to contact
 *)
let srvr_tftp host port =
  let conn = get_socket_connection host port in
  let he = gethostbyname host in
  let server_addr = he.h_addr_list.(0) in

  (* Using BOOTP to obtain connection details *)
  let bootp_msg = generate_bootp_request "" in
  let st_spi = (SNoOp (conn, ADDR_INET (server_addr, port)), SIPacket (ADDR_INET (server_addr, port), bootp_msg)) in
  do_server_step proto_srvr_next (Bytes.make 1024 '\000') st_spi
;;


(** Parsing of the command-line arguments. *)
let check_args() =
  if Array.length Sys.argv <> 5
  then
    (false,"Wrong number of arguments.",0)
  else if Sys.argv.(1) <> "-h" || Sys.argv.(3) <> "-p"
       then
          (false,"Wrong format of arguments.",0)
       else
         (try
            let port = int_of_string Sys.argv.(4) in
            (true,Sys.argv.(2), port)
          with Failure _ ->
            (false,"Problem in port value parsing.",0));;


let print_usage errmsg  =
  begin
    print_string errmsg;
    print_newline ();
    print_string "Usage: tftps -h <host> -p <port>";
    print_newline ();
  end;;

let print_hello host port =
  begin
    print_string("Starting TFTP server under the address ");
    print_string(host);
    print_string(":");
    print_int(port);
    print_newline ();
  end;;


(** The function main checks for correct command-line arguments and
   runs proper version of the client.  *)
let main () =
  let (ok, host_or_error, port) = check_args() in
  if not ok
  then
     print_usage(host_or_error)
  else
    begin
      print_hello host_or_error port;
      (* We can change the transfer mode at compile time. *)
      match (srvr_tftp host_or_error port) with
      | OK -> print_string "Done OK\n"
      | RunError strerr -> print_string ("Error: " ^ strerr ^ "\n")
    end
;;

(* Very simplistic start of the main function together with a test if
   the program runs in an interpreter.  *)
if !Sys.interactive then () else main ();;
