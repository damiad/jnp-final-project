open Unix;;

type bootp_packet = {
  op: char;
  htype: char;
  hlen: char;
  hops: char;
  xid: int32;
  secs: int;
  flags: int;
  ciaddr: inet_addr;
  yiaddr: inet_addr;
  siaddr: inet_addr;
  giaddr: inet_addr;
  chaddr: string;
  sname: string;
  file: string;
  options: string;
}

let serialize_bootp_packet packet =
  Bytes.of_string "This is a placeholder request"

let create_bootp_request () =
  {
    op = char_of_int 1; (* Request *)
    htype = char_of_int 1; (* Ethernet *)
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345;
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_any;
    siaddr = inet_addr_any;
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000"; (* MAC Address placeholder *)
    sname = "";
    file = "";
    options = "";
  }

let byte_to_int byte = Char.code byte

let bytes_to_ip_string bytes =
  let ip_parts = ref [] in
  for i = 0 to Bytes.length bytes - 1 do
    ip_parts := string_of_int (byte_to_int (Bytes.get bytes i)) :: !ip_parts
  done;
  String.concat "." (List.rev !ip_parts)

let bytes_to_int32 bytes =
  let open Int32 in
  let b0 = shift_left (of_int (Char.code (Bytes.get bytes 0))) 24 in
  let b1 = shift_left (of_int (Char.code (Bytes.get bytes 1))) 16 in
  let b2 = shift_left (of_int (Char.code (Bytes.get bytes 2))) 8 in
  let b3 = of_int (Char.code (Bytes.get bytes 3)) in
  logor (logor b0 b1) (logor b2 b3)

let bytes_to_int8 bytes =
  let open Int32 in
  of_int (Char.code (Bytes.get bytes 0))

let deserialize_bootp_response response =
  let op = byte_to_int (Bytes.get response 0) in
  let htype = byte_to_int (Bytes.get response 1) in
  let hlen = byte_to_int (Bytes.get response 2) in
  let hops = byte_to_int (Bytes.get response 3) in
  let xid = bytes_to_int32 (Bytes.sub response 4 4) in
  let secs = Bytes.get_uint16_be response 8 in
  let flags = Bytes.get_uint16_be response 10 in
  let ciaddr = Bytes.sub response 12 4 in
  let yiaddr = Bytes.sub response 16 4 in
  let siaddr = Bytes.sub response 20 4 in
  let giaddr = Bytes.sub response 24 4 in
  let chaddr = Bytes.sub response 28 16 in
  let sname = Bytes.sub response 44 64 in
  let file = Bytes.sub response 108 128 in
  (* let options = Bytes.sub response 236 (Bytes.length response - 236) in *)

  Printf.printf "op: %d, htype: %d, hlen: %d, hops: %d, xid: %ld, secs: %d, flags: %d\n"
    op htype hlen hops xid secs flags;
  Printf.printf "ciaddr: %s, yiaddr: %s, siaddr: %s, giaddr: %s\n"
    (bytes_to_ip_string ciaddr) (bytes_to_ip_string yiaddr) (bytes_to_ip_string siaddr) (bytes_to_ip_string giaddr);
  Printf.printf "chaddr: %s, sname: %s, file: %s\n"
    (Bytes.to_string chaddr) (Bytes.to_string sname) (Bytes.to_string file)
  (* Options can be handled differently *)

let print_bytes_as_hex bytes =
  for i = 0 to Bytes.length bytes - 1 do
    Printf.printf "%02x " (Char.code (Bytes.get bytes i))
  done;
  Printf.printf "\n"

let send_bootp_request () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_BROADCAST true;
  bind sock (ADDR_INET (inet_addr_any, 0));  (* Bind to a random free port *)

  let packet = create_bootp_request () in
  let packet_bytes = serialize_bootp_packet packet in
  let _ = sendto sock packet_bytes 0 (Bytes.length packet_bytes) [] (ADDR_INET (inet_addr_of_string "255.255.255.255", 67)) in
  Printf.printf "Sent BOOTP request\n";

  let response = Bytes.create 236 in
  let (num_bytes, _) = recvfrom sock response 0 236 [] in
  if num_bytes > 0 then begin
    Printf.printf "Received response:\n";
    (* print_bytes_as_hex response; *)
    deserialize_bootp_response response
  end else
    Printf.printf "No response received or error occurred\n"

let () = send_bootp_request ()

(* open Unix;;

type bootp_packet = {
  op: char;
  htype: char;
  hlen: char;
  hops: char;
  xid: int32;
  secs: int;
  flags: int;
  ciaddr: inet_addr;
  yiaddr: inet_addr;
  siaddr: inet_addr;
  giaddr: inet_addr;
  chaddr: string;
  sname: string;
  file: string;
  options: string;
}

let serialize_bootp_packet packet =
  let buffer = Bytes.create 236 in
  (* Fill the buffer with packet data *)
  buffer

let create_bootp_request () =
  {
    op = char_of_int 1; (* Request *)
    htype = char_of_int 1; (* Ethernet *)
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345; (* Transaction ID *)
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_any;
    siaddr = inet_addr_any;
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000"; (* MAC Address placeholder *)
    sname = "";
    file = "";
    options = "";
  }

let send_bootp_request () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_BROADCAST true;
  let packet = create_bootp_request () in
  let packet_bytes = serialize_bootp_packet packet in
  let _ = sendto sock packet_bytes 0 (Bytes.length packet_bytes) [] (ADDR_INET (inet_addr_of_string "255.255.255.255", 67)) in
  Printf.printf "Sent BOOTP request\n";;

let () = send_bootp_request () *)

(* open Unix;;

type bootp_packet = {
  op: char;
  htype: char;
  hlen: char;
  hops: char;
  xid: int32;
  secs: int;
  flags: int;
  ciaddr: inet_addr;
  yiaddr: inet_addr;
  siaddr: inet_addr;
  giaddr: inet_addr;
  chaddr: string;
  sname: string;
  file: string;
  options: string;
}

let serialize_bootp_packet packet =
  Bytes.of_string "This is a placeholder request"

let create_bootp_request () =
  {
    op = char_of_int 1;
    htype = char_of_int 1;
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345;
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_any;
    siaddr = inet_addr_any;
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000";
    sname = "";
    file = "";
    options = "";
  }

let send_bootp_request () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_BROADCAST true;
  let packet = create_bootp_request () in
  let packet_bytes = serialize_bootp_packet packet in
  let _ = sendto sock packet_bytes 0 (Bytes.length packet_bytes) [] (ADDR_INET (inet_addr_of_string "255.255.255.255", 67)) in
  Printf.printf "Sent BOOTP request\n";
  (* Odbieranie odpowiedzi (uproszczone) *)
  let response = Bytes.create 236 in
  let _ = recv sock response 0 236 [] in
  Printf.printf "Received response: %s\n" (Bytes.to_string response)

let () = send_bootp_request ()

open Unix;;

type bootp_packet = {
  op: char;
  htype: char;
  hlen: char;
  hops: char;
  xid: int32;
  secs: int;
  flags: int;
  ciaddr: inet_addr;
  yiaddr: inet_addr;
  siaddr: inet_addr;
  giaddr: inet_addr;
  chaddr: string;
  sname: string;
  file: string;
  options: string;
}

let serialize_bootp_packet packet =
  Bytes.of_string "This is a placeholder request"

let create_bootp_request () =
  {
    op = char_of_int 1;
    htype = char_of_int 1;
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345;
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_any;
    siaddr = inet_addr_any;
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000";
    sname = "";
    file = "";
    options = "";
  }


let send_bootp_request () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_BROADCAST true;
  bind sock (ADDR_INET (inet_addr_any, 0));  (* Bind to a random free port *)

  let packet = create_bootp_request () in
  let packet_bytes = serialize_bootp_packet packet in
  let _ = sendto sock packet_bytes 0 (Bytes.length packet_bytes) [] (ADDR_INET (inet_addr_of_string "255.255.255.255", 67)) in
  Printf.printf "Sent BOOTP request\n";

  let response = Bytes.create 236 in
  let (num_bytes, _) = recvfrom sock response 0 236 [] in
  if num_bytes > 0 then
    Printf.printf "Received response: %s\n" (Bytes.to_string response)
  else
    Printf.printf "No response received or error occurred\n"

let () = send_bootp_request () *)

(* open Unix;;

type bootp_packet = {
  op: char;
  htype: char;
  hlen: char;
  hops: char;
  xid: int32;
  secs: int;
  flags: int;
  ciaddr: inet_addr;
  yiaddr: inet_addr;
  siaddr: inet_addr;
  giaddr: inet_addr;
  chaddr: string;
  sname: string;
  file: string;
  options: string;
}

let serialize_bootp_packet packet =
  Bytes.of_string "This is a placeholder request"

let create_bootp_request () =
  {
    op = char_of_int 1;
    htype = char_of_int 1;
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345;
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_any;
    siaddr = inet_addr_any;
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000";
    sname = "";
    file = "";
    options = "";
  }

let print_bytes_as_hex bytes =
  for i = 0 to Bytes.length bytes - 1 do
    Printf.printf "%02x " (Char.code (Bytes.get bytes i))
  done;
  Printf.printf "\n"

let send_bootp_request () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_BROADCAST true;
  bind sock (ADDR_INET (inet_addr_any, 0));  (* Bind to a random free port *)

  let packet = create_bootp_request () in
  let packet_bytes = serialize_bootp_packet packet in
  let _ = sendto sock packet_bytes 0 (Bytes.length packet_bytes) [] (ADDR_INET (inet_addr_of_string "255.255.255.255", 67)) in
  Printf.printf "Sent BOOTP request\n";

  let response = Bytes.create 236 in
  let (num_bytes, _) = recvfrom sock response 0 236 [] in
  if num_bytes > 0 then begin
    Printf.printf "Received response:\n";
    print_bytes_as_hex response
  end else
    Printf.printf "No response received or error occurred\n"

let () = send_bootp_request () *)
(* 
open Unix;;

type bootp_packet = {
  op: char;
  htype: char;
  hlen: char;
  hops: char;
  xid: int32;
  secs: int;
  flags: int;
  ciaddr: inet_addr;
  yiaddr: inet_addr;
  siaddr: inet_addr;
  giaddr: inet_addr;
  chaddr: string;
  sname: string;
  file: string;
  options: string;
}

let serialize_bootp_packet packet =
  Bytes.of_string "This is a placeholder request"

let create_bootp_request () =
  {
    op = char_of_int 1; (* Request *)
    htype = char_of_int 1; (* Ethernet *)
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345;
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_any;
    siaddr = inet_addr_any;
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000"; (* MAC Address placeholder *)
    sname = "";
    file = "";
    options = "";
  }

let byte_to_int byte = Char.code byte

let bytes_to_ip_string bytes =
  let ip_parts = ref [] in
  for i = 0 to Bytes.length bytes - 1 do
    ip_parts := string_of_int (byte_to_int (Bytes.get bytes i)) :: !ip_parts
  done;
  String.concat "." (List.rev !ip_parts)

(* let deserialize_bootp_response response =
  let yiaddr_bytes = Bytes.sub response 16 4 in
  let siaddr_bytes = Bytes.sub response 20 4 in

  let yiaddr_str = bytes_to_ip_string yiaddr_bytes in
  let siaddr_str = bytes_to_ip_string siaddr_bytes in

  Printf.printf "Assigned IP (yiaddr): %s\n" yiaddr_str;
  Printf.printf "Server IP (siaddr): %s\n" siaddr_str *)

let bytes_to_int32 bytes =
  let open Int32 in
  let b0 = shift_left (of_int (Char.code (Bytes.get bytes 0))) 24 in
  let b1 = shift_left (of_int (Char.code (Bytes.get bytes 1))) 16 in
  let b2 = shift_left (of_int (Char.code (Bytes.get bytes 2))) 8 in
  let b3 = of_int (Char.code (Bytes.get bytes 3)) in
  logor (logor b0 b1) (logor b2 b3)

(* let deserialize_bootp_response response =
  (* Odczytywanie i interpretacja pól z odpowiedzi, na przykład: *)
  let xid = bytes_to_int32 (Bytes.sub response 4 4) in
  Printf.printf "Transaction ID (xid): %ld\n" xid;
  (* ... *)
  (* Twoje dotychczasowe odczytywanie yiaddr i siaddr *)
  let yiaddr_str = bytes_to_ip_string (Bytes.sub response 16 4) in
  let siaddr_str = bytes_to_ip_string (Bytes.sub response 20 4) in
  Printf.printf "Assigned IP (yiaddr): %s\n" yiaddr_str;
  Printf.printf "Server IP (siaddr): %s\n" siaddr_str *)

let deserialize_bootp_response response =
  let op = Bytes.get response 0 in
  let htype = Bytes.get response 1 in
  let hlen = Bytes.get response 2 in
  let hops = Bytes.get response 3 in
  let xid = bytes_to_int32 (Bytes.sub response 4 4) in
  let secs = Bytes.get_uint16_be response 8 in
  let flags = Bytes.get_uint16_be response 10 in
  let ciaddr = Bytes.sub response 12 4 in
  let yiaddr = Bytes.sub response 16 4 in
  let siaddr = Bytes.sub response 20 4 in
  let giaddr = Bytes.sub response 24 4 in
  let chaddr = Bytes.sub response 28 16 in
  let sname = Bytes.sub response 44 64 in
  let file = Bytes.sub response 108 128 in
  let options = Bytes.sub response 236 (Bytes.length response - 236) in

  Printf.printf "op: %c, htype: %c, hlen: %c, hops: %c, xid: %ld, secs: %d, flags: %d\n"
    op htype hlen hops xid secs flags;
  Printf.printf "ciaddr: %s, yiaddr: %s, siaddr: %s, giaddr: %s\n"
    (bytes_to_ip_string ciaddr) (bytes_to_ip_string yiaddr) (bytes_to_ip_string siaddr) (bytes_to_ip_string giaddr);
  Printf.printf "chaddr: %s, sname: %s, file: %s\n"
    (Bytes.to_string chaddr) (Bytes.to_string sname) (Bytes.to_string file);
  (* Opcje można zinterpretować w zależności od ich formatu i zawartości *)

let bytes_to_int32 bytes =
  let open Int32 in
  let b0 = shift_left (of_int (Char.code (Bytes.get bytes 0))) 24 in
  let b1 = shift_left (of_int (Char.code (Bytes.get bytes 1))) 16 in
  let b2 = shift_left (of_int (Char.code (Bytes.get bytes 2))) 8 in
  let b3 = of_int (Char.code (Bytes.get bytes 3)) in
  logor (logor b0 b1) (logor b2 b3)

let print_bytes_as_hex bytes =
  for i = 0 to Bytes.length bytes - 1 do
    Printf.printf "%02x " (Char.code (Bytes.get bytes i))
  done;
  Printf.printf "\n"

let send_bootp_request () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_BROADCAST true;
  bind sock (ADDR_INET (inet_addr_any, 0));  (* Bind to a random free port *)

  let packet = create_bootp_request () in
  let packet_bytes = serialize_bootp_packet packet in
  let _ = sendto sock packet_bytes 0 (Bytes.length packet_bytes) [] (ADDR_INET (inet_addr_of_string "255.255.255.255", 67)) in
  Printf.printf "Sent BOOTP request\n";

  let response = Bytes.create 236 in
  let (num_bytes, _) = recvfrom sock response 0 236 [] in
  if num_bytes > 0 then begin
    Printf.printf "Received response:\n";
    print_bytes_as_hex response;
    deserialize_bootp_response response
  end else
    Printf.printf "No response received or error occurred\n"

let () = send_bootp_request () *)
