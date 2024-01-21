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

(* let inet_addr_to_bytes addr =
  let addr_long = (Obj.magic addr : int32) in
  let bytes = Bytes.create 4 in
  Bytes.set bytes 0 (Char.chr (Int32.to_int (Int32.shift_right_logical addr_long 24) land 0xFF));
  Bytes.set bytes 1 (Char.chr (Int32.to_int (Int32.shift_right_logical addr_long 16) land 0xFF));
  Bytes.set bytes 2 (Char.chr (Int32.to_int (Int32.shift_right_logical addr_long 8) land 0xFF));
  Bytes.set bytes 3 (Char.chr (Int32.to_int addr_long land 0xFF));
  bytes *)

let inet_addr_to_bytes addr =
  if addr = Unix.inet_addr_any then
    Bytes.of_string "\000\000\000\000"
  else
    let addr_str = Unix.string_of_inet_addr addr in
    let addr_parts = String.split_on_char '.' addr_str in
    let addr_bytes = List.map int_of_string addr_parts in
    let bytes = Bytes.create 4 in
    List.iteri (fun i b -> Bytes.set bytes i (char_of_int b)) addr_bytes;
    bytes


let inet_addr_of_string str =
  Unix.inet_addr_of_string str

let create_bootp_packet () =
  {
    op = char_of_int 2; (* Reply *)
    htype = char_of_int 1; (* Ethernet *)
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345; (* Transaction ID *)
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_of_string "192.168.1.2"; (* Assigned IP *)
    siaddr = inet_addr_of_string "192.168.1.1"; (* Server IP *)
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000"; (* Client MAC Address *)
    sname = "";
    file = "";
    options = "";
  }

let int32_to_bytes n =
  let open Int32 in
  let b0 = to_int (logand n 0xFFl) |> Char.chr in
  let b1 = to_int (logand (shift_right_logical n 8) 0xFFl) |> Char.chr in
  let b2 = to_int (logand (shift_right_logical n 16) 0xFFl) |> Char.chr in
  let b3 = to_int (logand (shift_right_logical n 24) 0xFFl) |> Char.chr in
  Bytes.of_string (String.make 1 b3 ^ String.make 1 b2 ^ String.make 1 b1 ^ String.make 1 b0)

let serialize_bootp_packet packet =
  let buffer = Bytes.create 236 in
  Bytes.set buffer 0 packet.op;
  Bytes.set buffer 1 packet.htype;
  Bytes.set buffer 2 packet.hlen;
  Bytes.set buffer 3 packet.hops;
  
  let xid_bytes = int32_to_bytes packet.xid in
  Bytes.blit xid_bytes 0 buffer 4 4;

  let secs_bytes = Bytes.of_string (String.make 2 (Char.chr packet.secs)) in
  Bytes.blit secs_bytes 0 buffer 8 2;

  let flags_bytes = Bytes.of_string (String.make 2 (Char.chr packet.flags)) in
  Bytes.blit flags_bytes 0 buffer 10 2;

  let ciaddr_bytes = inet_addr_to_bytes packet.ciaddr in
  Bytes.blit ciaddr_bytes 0 buffer 12 4;

  let yiaddr_bytes = inet_addr_to_bytes packet.yiaddr in
  Bytes.blit yiaddr_bytes 0 buffer 16 4;

  let siaddr_bytes = inet_addr_to_bytes packet.siaddr in
  Bytes.blit siaddr_bytes 0 buffer 20 4;

  let giaddr_bytes = inet_addr_to_bytes packet.giaddr in
  Bytes.blit giaddr_bytes 0 buffer 24 4;

  let chaddr_bytes = Bytes.of_string (String.make 16 (Char.chr 0)) in
  Bytes.blit (Bytes.of_string packet.chaddr) 0 chaddr_bytes 0 (String.length packet.chaddr);
  Bytes.blit chaddr_bytes 0 buffer 28 16;

  (* Fill sname, file i options *)
  (* ... *)

  buffer


let handle_bootp_request _ =
  Printf.printf "Received BOOTP request\n";
  create_bootp_packet ()

let bootp_server () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock (ADDR_INET (inet_addr_any, 67));
  while true do
    let buf = Bytes.create 236 in
    let (_, sender) = recvfrom sock buf 0 236 [] in
    let reply_packet = handle_bootp_request (Bytes.to_string buf) in
    let reply = serialize_bootp_packet reply_packet in
    let _ = sendto sock reply 0 (Bytes.length reply) [] sender in ()
  done

let () = bootp_server ()

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

let inet_addr_of_string str =
  Unix.inet_addr_of_string str

let create_bootp_packet () =
  {
    op = char_of_int 2; (* Reply *)
    htype = char_of_int 1; (* Ethernet *)
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345; (* Transaction ID *)
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_of_string "192.168.1.2"; (* Assigned IP *)
    siaddr = inet_addr_of_string "192.168.1.1"; (* Server IP *)
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000"; (* Client MAC Address *)
    sname = "";
    file = "";
    options = "";
  }

let serialize_bootp_packet packet =
  let buffer = Bytes.create 236 in
  (* Fill the buffer with packet data *)
  (* This is a placeholder. You'll need to implement the logic to fill the buffer. *)
  buffer

let handle_bootp_request packet =
  Printf.printf "Received BOOTP request\n";
  let reply_packet = create_bootp_packet () in
  serialize_bootp_packet reply_packet

let bootp_server () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock (ADDR_INET (inet_addr_any, 67));
  while true do
    let buf = Bytes.create 236 in
    let (_, sender) = recvfrom sock buf 0 236 [] in
    let reply = handle_bootp_request (Bytes.to_string buf) in
    let _ = sendto sock reply 0 (Bytes.length reply) [] sender in ()
  done

let () = bootp_server () *)

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

let inet_addr_of_string str =
  Unix.inet_addr_of_string str

let create_bootp_packet () =
  {
    op = char_of_int 2;
    htype = char_of_int 1;
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345;
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_of_string "192.168.1.2";
    siaddr = inet_addr_of_string "192.168.1.1";
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000";
    sname = "";
    file = "";
    options = "";
  }

let serialize_bootp_packet packet =
  Bytes.of_string "This is a placeholder response"

let handle_bootp_request packet =
  Printf.printf "Received BOOTP request: %s\n" packet;
  create_bootp_packet ()

let bootp_server () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock (ADDR_INET (inet_addr_any, 67));
  while true do
    let buf = Bytes.create 236 in
    let (_, sender) = recvfrom sock buf 0 236 [] in
    Printf.printf "Received data from client\n";
    let reply_packet = handle_bootp_request (Bytes.to_string buf) in
    let reply = serialize_bootp_packet reply_packet in
    let _ = sendto sock reply 0 (Bytes.length reply) [] sender in ()
  done

let () = bootp_server () *)

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

let inet_addr_of_string str =
  Unix.inet_addr_of_string str

let create_bootp_packet () =
  {
    op = char_of_int 2; (* Reply *)
    htype = char_of_int 1; (* Ethernet *)
    hlen = char_of_int 6;
    hops = char_of_int 0;
    xid = Int32.of_int 12345; (* Transaction ID *)
    secs = 0;
    flags = 0;
    ciaddr = inet_addr_any;
    yiaddr = inet_addr_of_string "192.168.1.2"; (* Assigned IP *)
    siaddr = inet_addr_of_string "192.168.1.1"; (* Server IP *)
    giaddr = inet_addr_any;
    chaddr = "\000\000\000\000\000\000"; (* Client MAC Address *)
    sname = "";
    file = "";
    options = "";
  }

let serialize_bootp_packet packet =
  (* Tutaj powinna znaleźć się logika do prawidłowej serializacji pakietu BOOTP *)
  Bytes.of_string "This is a placeholder response"

let deserialize_bootp_packet data =
  (* Tutaj powinna znaleźć się logika do deserializacji przychodzącego pakietu BOOTP *)
  Printf.printf "Received data: %s\n" data;
  create_bootp_packet ()

let handle_bootp_request data =
  let packet = deserialize_bootp_packet data in
  serialize_bootp_packet packet

let bootp_server () =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock (ADDR_INET (inet_addr_any, 67));
  while true do
    let buf = Bytes.create 236 in
    let (_, sender) = recvfrom sock buf 0 236 [] in
    Printf.printf "Received BOOTP request\n";
    let reply = handle_bootp_request (Bytes.to_string buf) in
    let _ = sendto sock reply 0 (Bytes.length reply) [] sender in ()
  done

let () = bootp_server () *)
