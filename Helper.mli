open ProtocolDefinitions

val init_upload_connection :
  file_descr -> string -> string -> string -> sockaddr ->
  coq_PCState * coq_POutput

val init_download_connection :
  file_descr -> string -> string -> string -> sockaddr ->
  coq_PCState * coq_POutput

val communication_timeout : coq_CProtoData -> coq_PCState * coq_POutput
