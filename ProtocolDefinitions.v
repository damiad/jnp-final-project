Require Import String.
Require Import ZArith.
Require Import Int63.Sint63.
Require Import PArray.
Require Import Ascii String Coq.Strings.Byte.
Require Import Lists.List.
Import ListNotations.
Open Scope list_scope.

Variable file_descr : Set.
Variable sockaddr : Set.

Inductive bootp_message : Set :=
| BOOTREQUEST
| BOOTREPLY.

Inductive CPInput : Set :=
| Init (conn:file_descr) (lfname:string) (rfname:string) (mode:string) (saport:sockaddr)
| IPacket (content:string) (plen:int)
| PIError (reason:string).

Inductive SPInput : Set :=
| SIPacket (addr:sockaddr) (buf:string)
| SPIError (reason:string)
| SNothing.

Definition CProtoData : Set := file_descr * int * bootp_message.

Inductive PCState : Set :=
| NoOp
| FComm (pd:CProtoData) (is_timeout:bool)
| Final
| SError (reason:string).

Definition SProtoData : Set :=
  ((list (sockaddr * int)) * (list file_descr) * sockaddr * file_descr * bootp_message).

Inductive PSState : Set :=
| SNoOp (conn:file_descr) (addr:sockaddr)
| SFComm (pd:SProtoData) (is_timeout:bool)
| SFinal
| SSError (reason:string).

Inductive POutput : Set :=
| OPacket (content:string)
| Nothing.

Inductive Status : Set :=
| OK
| RunError (reason:string).

Inductive Packet : Set :=
| BOOTPRequest (fields: list string)
| BOOTPReply (fields: list string).

