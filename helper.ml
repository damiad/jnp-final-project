open Unix
open Uint63
open String
open ProtocolDefinitions

let init_upload_connection (conn: file_descr) (lfn: string)
      (rfn: string) (mname: string) (server: sockaddr) =
  let payload = lfn ^ "-" ^ rfn ^ "-" ^ mname in
  let _ = sendto conn (String.to_bytes payload) 0 (String.length payload) [] server in
  let three = Uint63.of_int 3 in
  (FComm ((conn, three), false), OPacket payload)

let init_download_connection (conn: file_descr) lfn rfn mname server =
  (SError "File download using BOOTP is not implemented", Nothing)

let communication_timeout (pd: coq_CProtoData) =
  (SError "Timeout handling for BOOTP is not implemented", Nothing)



