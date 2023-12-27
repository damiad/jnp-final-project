val init_upload_connection :
  ProtocolDefinitions.file_descr ->
  string ->
  string ->
  string ->
  ProtocolDefinitions.sockaddr ->
  ProtocolDefinitions.coq_PCState * ProtocolDefinitions.coq_POutput
val init_download_connection :
  'a ->
  'b ->
  'c ->
  'd ->
  'e -> ProtocolDefinitions.coq_PCState * ProtocolDefinitions.coq_POutput
val communication_timeout :
  'a -> ProtocolDefinitions.coq_PCState * ProtocolDefinitions.coq_POutput
