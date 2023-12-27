Require Import ZArith. (** Necessary for Lia *)
Require Import Lia. (** Necessary for lia *)
Require Import Int63.Sint63.
Require Import PArray.
Require Import Ascii String Coq.Strings.Byte.
Require Import Lists.List.
Import ListNotations.
Open Scope list_scope.


Require Import ssreflect ssrbool.

(** We need compiled versions of the files with the modules.
Therefore we make the compilation first. *)
Require Import ProtocolDefinitions.
Require Import Helper.
Include Helper.


Lemma string_len_zero:
  forall s,
    String.length s = 0 -> s = ""%string.
Proof.
  intros.
  (* Proof by cases depending on s *)
  destruct s.
  * auto.
  * simpl in H.
    inversion H.
Qed.

Lemma prefix_empty:
  forall s,
    prefix "" s = true.
Proof.
  intros.
  (* Proof by cases; all cases are very simple so one
     way to prove them is enough. *)
  destruct s; simpl;auto.
Qed.

Lemma prefix_different:
  forall a a' str str1,
    a <> a' ->
    prefix (String a str) (String a' str1) = false.
Proof.
  intros.
  simpl.
  Import Arith.
  (* Proof by cases depending on the result of expression
     in if. *)
  destruct (ascii_dec a a');
    (* We see that one of the cases is proved by contradiction. *)
    try contradiction.
  trivial.
Qed.

Lemma prefix_len:
  forall s1 s2,
    prefix s1 s2 = true ->
    String.length s1 <= String.length s2.
Proof.
  induction s1.
  * intros. simpl.
    lia.
  * intros.
    destruct s2;simpl in *;try congruence.
    destruct (ascii_dec a a0); try congruence.
    subst a0.
    apply IHs1 in H.
    lia.
Qed.


Lemma full_substring:
  forall str,
    str = substring 0 (String.length str) str.
Proof.
  induction str.
  * simpl;trivial.
  * simpl.
    (* We use f_equal when a constructor (String this time) is applied
       on both sides of an equality *)
    f_equal.
    trivial.
Qed.

Lemma substring_string:
  forall str n a,
    substring 0 (S n) (String a str) =
      String a (substring 0 n str).
Proof.
  destruct str.
  * intros.
    simpl.
    destruct n; auto.
  * intros.
    unfold substring at 1.
    fold substring.
    destruct n.
    ** simpl; trivial.
    ** simpl. f_equal.
Qed.



Lemma substring_skip_app:
  forall fs p k snd,
    substring (String.length fs + p) k (fs ++ snd) =
      substring  p k snd.
Proof.
  induction fs.
  * intros.
    now simpl.
  * intros.
    simpl.
    apply IHfs.
Qed.

Lemma string_app:
  forall str str1 a,
    ((String a str) ++ str1  = String a (str ++ str1))%string.
Proof.
  induction str; intros; now simpl.
Qed.

Lemma string_app_length:
  forall str str1,
    String.length (str ++ str1)  = String.length str + String.length str1.
Proof.
  (** Incudtion usually works best at the first part of an expression *)
  induction str.
  * intros.
    now simpl.
  * intros.
    simpl.
    now rewrite IHstr. 
Qed.


Lemma string_app_empty:
  forall str,
    (str ++ "" = str)%string.
Proof.
  induction str.
  * now simpl.
  * rewrite string_app.
    now rewrite IHstr.
Qed.

Lemma string_app_assoc:
  forall a b c,
    (a ++ b ++ c = (a ++ b) ++ c)%string.
Proof.
  induction a.
  * now simpl.
  * intros. rewrite string_app.
    now rewrite IHa.
Qed.


Lemma substring_zero_zero:
  forall str,
    substring 0 0 str = ""%string.
Proof.
  destruct str; now simpl.
Qed.

Lemma substring_length:
  forall str str1,
    str = substring 0 (String.length str) (str ++ str1).
Proof.
  induction str.
  * intros.
    simpl.
    now rewrite substring_zero_zero.
  * intros. simpl.
    now f_equal.
Qed.

Lemma length_of_substring:
  forall str k l,
    String.length str >= k + l ->
    String.length (substring k l str) = l.
Proof.
  induction str.
  * intros.
    simpl in H.
    simpl.
    destruct k;try lia.
    destruct l;try lia.
    now simpl.
  * intros.
    simpl.
    destruct k.
    ** destruct l.
       *** now simpl.
       *** simpl.
           f_equal.
           apply IHstr.
           simpl in H;lia.
    ** apply IHstr.
       simpl in H;lia.
Qed.


Lemma substring_app_skip:
  forall fs sn k,
    substring (String.length fs) k (fs ++ sn)%string =
      substring 0 k sn.
Proof.
  induction fs.
  * intros.
    now simpl.
  * intros.
    simpl.
    now rewrite IHfs.
Qed.
    
Lemma index_eq:
  forall a str n,
    index 0 (String a "") (String a str) = Some n ->
    String a str =
      (substring 0 n (String a str) ++
         String a "" ++
         (substring (n + 1) (String.length (String a str) - n - 1) (String a str)))%string.
Proof.
  intros.
  simpl in H.
  (* We evaluate if, by cases. One of them is obious and discharged by
     congruence. We have to use try becasue only one branch is
     discharged by congruence. *)
  destruct (ascii_dec a a); try congruence.
  rewrite prefix_empty in H.
  (* We unpack 0 = n out of applied Some *)
  inversion H.
  (* Replace all occurrences of n by the other side of equality in
     assumptins *)
  subst n.
  simpl.
  f_equal.
  (* We replace arithmetical expression with something simpler,
     but we have to proove the equality. The arithmetical part
     is done by lia tactic. *)
  replace  (String.length str - 0) with (String.length str) by lia.
  replace (str) with (str ++ "")%string at 3.
  * apply substring_length.
  * now rewrite string_app_empty.
Qed.  

Lemma index_neq:
  forall a a' str n,
    a <> a' ->
    index 0 (String a "") (String a' str) = Some (S n) ->
    substring 0 (S n) (String a str) = String a (substring 0 n str).
Proof. 
  induction str.
  * intros.
    unfold index in H0.
    rewrite prefix_different in H0; congruence.
  * intros.
    set (sstr := (String a0 str)).
    unfold index in  H0.
    fold index in H0.
    generalize prefix_different; intro.
    replace (prefix (String a "") (String a' (String a0 str))) with false in H0.
    **  pose (index 0 (String a "") (String a0 str)).
        unfold index in o.
        fold index in o.
        fold o in H0.
        destruct o;try congruence.
        (* We remove the constructors in equality *)
        injection H0;intros.
        subst n.
        now simpl.
    ** simpl.
       destruct (ascii_dec a a'); try congruence.
Qed.

Lemma prefix_app_neg:
  forall str c str1,
    prefix c (str ++ str1) = false ->
    prefix c str = false.
Proof.
  induction str.
  * intros.
    simpl.
    destruct c; auto.
    simpl in H.
    destruct str1; simpl in H; try congruence.
  * intros.
    simpl in H.
    destruct c; try congruence.
    simpl.
    destruct (ascii_dec a0 a);try congruence.
    eauto.
Qed.


Lemma index_app:
  forall fs sn,
    index 0 ":" fs = None ->
    index 0 ":" (fs ++ ":" ++ sn) = Some (String.length fs).
Proof.
  induction fs.
  * intros.
    simpl.
    now rewrite prefix_empty.
  * intros.
    rewrite string_app.
    unfold index in H.
    fold index in H.
    unfold index.
    fold index.
    destruct (prefix ":" (String a fs)) eqn: Pfx;try congruence.
    ** destruct (index 0 ":" fs) eqn: Idx;try congruence.
       rewrite IHfs;auto.
       unfold prefix in Pfx.
       fold prefix in Pfx.
       destruct (ascii_dec ":" a).
       *** subst a.
           rewrite prefix_empty in Pfx; try congruence.
       *** rewrite prefix_different;auto.
Qed.

Lemma index_succ_ge_zero:
  forall p c a str n,
    index (S p) c (String a str) = Some n ->
    n >0.
Proof.
  intros.
  destruct n.
  * simpl in H.
    destruct (index p c str); congruence.
  * auto with zarith.
Qed.


Lemma index_succ_no_succ:
  forall p c a str n,
    index (S p) c (String a str) = Some (S n) ->
    index p c str = Some n.
Proof.
  intros.
  simpl in H. 
  destruct (index p c str) eqn:Idx; congruence.
Qed.

Lemma index_succ_eq1:
  forall p c a fs n,
    index (S p) c (String a fs) = Some (S n) ->
    index p c fs = Some n.
Proof.
  intros.
  simpl in H.
  destruct (index p c fs); congruence.
Qed.  




Lemma index_succ_eq2:
  forall p c a fs,
    index (S p) c (String a fs) = None ->
    index p c fs = None.
Proof.
  intros.
  simpl in H.
  destruct (index p c fs);congruence.
Qed.


Lemma index_succ_eq3:
  forall c a fs n,
    index 0 c (String a fs) = Some (S n) ->
    index 0 c fs = Some n.
Proof.
  intros.
  simpl in H.
  destruct c;try congruence.
  destruct (ascii_dec a0 a).
  * subst a0.
    destruct (prefix c fs) eqn: Pfx;try congruence.
    destruct (index 0 (String a c) fs) eqn: Idx;try congruence.
  * destruct (index 0 (String a0 c) fs); try congruence.
Qed.  


Lemma index_app_app:
  forall fs x p snd n,
  index (String.length fs + p) x (fs ++ snd) = Some n ->
    (index p x snd) = Some (n - (String.length fs)).
Proof.
  induction fs.
  * intros.
    simpl in H.
    rewrite H.
    simpl.
    now replace (n-0) with n by lia.
  * intros.
    replace  (String.length (String a fs) + p) with
      (S (String.length fs + p)) in H by now simpl.
    destruct n.
    ** simpl in H.
       destruct (index (String.length fs + p) x (fs ++ snd)); congruence.
    ** apply index_succ_eq1 in H.
       fold append in H.
       apply IHfs in H.
       rewrite H.
       f_equal.
Qed.

Lemma index_len_app:
  forall fs p c rs,
    index (String.length fs + p) c (fs ++ rs) = None ->
    index  p c  rs = None.
Proof.
  induction fs.
  * now simpl.
  * intros.
    simpl in H.
    destruct (index (String.length fs + p) c (fs ++ rs)) eqn: Idx.
    ** inversion H.
    ** now apply IHfs in Idx.
Qed.

Lemma index_len_bound:
  forall s k c n,
    index k c s = Some n ->
    n + String.length c <= String.length s. 
Proof.
  induction s, k.
  * intros.
    simpl in H.
    destruct c;try congruence.
    simpl.
    injection H;intros;lia.
  * intros.
    simpl in H.
    congruence.
  * intros. 
    simpl in H.
    destruct c.
    ** injection H;intros; subst n.
       simpl.
       lia.
    ** destruct (ascii_dec a0 a).
       *** subst a0.
           destruct (prefix c s) eqn:Pfx.
           **** injection H;intros.
                subst n.
                simpl.
                apply prefix_len in Pfx.
                lia.
           **** destruct (index 0 (String a c) s) eqn: idx;try congruence.
                apply IHs in idx.
                injection H; intros.
                subst n.
                simpl in idx.
                simpl.
                lia.
       *** destruct (index 0 (String a0 c) s) eqn: idx; try congruence.
           apply IHs in idx.
           simpl in *.
           injection H; intros.
           lia.
  * intros.
    destruct n.
    ** simpl in H.
       destruct (index k c s) eqn: idx;try congruence.
    ** apply index_succ_no_succ in H.
       apply IHs in H.
       simpl in *.
       lia.
Qed.
       
Lemma substring_succ:
  forall p a l str,
    substring (S p) l (String a str) =
      substring p l str.
Proof.
  intros.
  now simpl.
Qed.


Lemma prefix_app:
  forall str c str1,
    prefix c str = true ->
    prefix c (str ++ str1) = true.
Proof.
  induction str.
  * intros;simpl.
    simpl in H.
    destruct c.
    ** apply prefix_empty.
    ** congruence.
  * intros.
    destruct c.
    ** now simpl.
    ** simpl.
       destruct (ascii_dec a0 a).
       *** subst a0.
           simpl in H.
           destruct (ascii_dec a a);try congruence.
           auto.
       *** simpl in H.
           destruct (ascii_dec a0 a); try congruence.
Qed.



  
Lemma substring_app_substring:
  forall str k,
    str = ((substring 0 k str) ++
            (substring k (String.length str - k) str))%string.
Proof.
  induction str.
  * intros.
    simpl.
    destruct k; now simpl.
  * intros.
    unfold substring at 1.
    fold substring.
    destruct k.
    ** simpl.
       now rewrite <- full_substring.
    ** simpl.
       f_equal.
       apply IHstr.
Qed.

Lemma index_app_first:
  forall c st ed,
    String.length c > 0 ->
    index 0 c (st ++ ed) = Some (String.length st) ->
    index 0 c st = None.
Proof.
  induction st.
  * intros.
    simpl.
    destruct c;simpl in H; try lia; auto.
  * intros.
    simpl.
    destruct c;simpl in H; try lia; auto.
    destruct (ascii_dec a0 a).
    ** subst a0.
       simpl in H0.
       destruct (ascii_dec a a); try congruence.
       destruct (prefix c (st ++ ed)) eqn:Pfx. 
       *** injection H0;intros;lia.
       *** erewrite prefix_app_neg;eauto.
           destruct (index 0 (String a c) (st ++ ed)) eqn:Idx; try congruence.
           injection H0;intros.
           subst n.
           erewrite IHst; eauto.
    ** simpl in H0.
       destruct (ascii_dec a0 a); try congruence.
       destruct (index 0 (String a0 c) (st ++ ed)) eqn: Idx; try congruence.
       injection H0;intros.
       subst n1.
       erewrite IHst;eauto.
Qed.         


Lemma to_index_no_occ_zero:
  forall str content c rpos,
    String.length c > 0 ->
    index 0 c content = Some rpos ->
    str = substring 0 rpos content ->
    index 0 c str = None.
Proof.
  intros.
  generalize H0;intros.
  erewrite (substring_app_substring content rpos) in H0.
  replace rpos with (String.length str) in H0 at 4.
  * rewrite <- H1 in H0.
    apply index_app_first in H0;auto.
  * rewrite H1.
    apply length_of_substring.
    apply index_len_bound in H2.
    lia.
Qed.
    

Lemma substring_string1:
  forall cont str k n a ,
    String a str = substring k (S n) cont ->
    substring (S k) n cont = str.
Proof.
  induction cont.
  * intros.
    simpl in *.
    destruct k;try congruence.
  * intros.
    simpl.
    simpl in H.
    destruct k;try congruence.
    now apply IHcont in H.
Qed.


Lemma len_of_colon:
  String.length ":"%string > 0.
Proof.
  simpl.
  lia.
Qed.

(** There are two forms of packet:
  - with separator ":" that divides data
    to chunk before the first ":" and after the first ":"
  - with no occurrence of ":" that contains only one
    chunk of data *)
Definition parse_content (content:string) :=
  let pos := index 0 ":"%string content in
  match pos with
  | Some rpos =>
      let f := substring 0 rpos content in
      let pos2 := index (rpos+1) ":"%string content in
      match pos2 with
      | Some rpos2 =>
          let s := substring (rpos+1)
                     (rpos2 - rpos - 1) content in
          let t := substring (rpos2+1)
                     ((String.length content) - rpos2 - 1) content in
          Divided f s t
      | None => NonDivided content
      end
  | None => NonDivided content
end.

Definition print_content (pckt:Packet) :=
  match pckt with
  | Divided f s t => (f ++ ":" ++ s ++ ":" ++ t)%string
  | NonDivided d => d
  end.

(** Tactics Simple https://www.cs.cornell.edu/courses/cs3110/2018sp/a5/coq-tactics-cheatsheet.html *)

Definition mystring_of_num (num:int) :string :=
  if (num =? 0)%uint63
  then "0"%string
  else if (num =? 1)%uint63
       then "1"%string
       else if (num =? 2)%uint63
            then "2"%string
            else "other"%string.


(** Generate reply in client *)
Definition communication_reply (pd:CProtoData) (inp:CPInput) :=
  match inp with
  | IPacket content plen =>
      let pcontent := parse_content content in
      match pcontent with
      | BOOTPReply fields =>
          let (desc, num, _) := pd in
          let mione := (num-1)%uint63 in 
          if (num =? 0)%uint63
          then (Final, Nothing)
          else (FComm (desc, mione, BOOTREPLY) false,
                 OPacket (bootp_reply_to_string (BOOTPReply fields) ++ ":" ++ (mystring_of_num mione))%string)
      | BOOTPRequest => (Final, Nothing)
      end
  | PIError reason => (SError reason, Nothing)
  | Init _ _ _ _ _ _ =>
    (SError "Initialisation data not allowed during communication",
      Nothing)
  end.


(** Function makes the transfer from the current state and input of the
    protocol to the next state and relevant output in case of the client. *)
Definition proto_next (st : PCState) (inp:CPInput) : (PCState * POutput) :=
  match st with
  | NoOp => (** initialization of the communication *)
      (match inp with
       | Init Upload conn lfn rfn mname saddr =>
          init_upload_connection conn lfn rfn mname saddr
       | Init Download conn lfn rfn mname saddr =>
           init_download_connection conn lfn rfn mname saddr
       | IPacket _ _ =>
          (SError "Packet is invalid at initialization", Nothing)
       | PIError msg => (SError msg, Nothing)
       end)
  | FComm pd tm => (** normal communication *)
     if tm
     then (* handling of the timeout case *)
       communication_timeout pd 
     else (* handling protocol reply *)
       communication_reply pd inp
  | Final => (* We assume that the decision to finalize is on the side of
                the protocol step, so this case should not take place  *)
     (SError "Wrong protocol handling", Nothing) 
  | SError msg => (** error *)
      (* Nothing can be done here. *)
      (SError msg, Nothing)
  end.

Definition update_condata_raw
  (condata:list (ProtocolDefinitions.sockaddr * int))
  (sad:ProtocolDefinitions.sockaddr) :=
  flat_map (fun (x:(ProtocolDefinitions.sockaddr * int)) =>
              let (sa, i) := x in
              if ProtocolDefinitions.eqb_sockaddr sa sad
              then [(sa, (i+1)%uint63)]
              else [(sa, i)]) condata.

Definition remove_condata
  (condata:list (ProtocolDefinitions.sockaddr * int))
  (sad:ProtocolDefinitions.sockaddr) :=
  fold_left (fun (acc:list (ProtocolDefinitions.sockaddr * int))
                 (el:(ProtocolDefinitions.sockaddr * int)) =>
               let (sa, i) := el in
               if ProtocolDefinitions.eqb_sockaddr sa sad
               then acc
               else (sa,i) :: acc)
    condata [].

(** Convert BOOTPReply fields to string representation *)
Definition bootp_reply_to_string (fields: BOOTPReply) : string :=
  match fields with
  | MkBOOTPReply op htype hlen hops xid secs flags ciaddr yiaddr siaddr giaddr chaddr sname file bop =>
      (* Assuming bop is a string *)
      String (Char.unicode_of_ascii op) ""
      ++ String (Char.unicode_of_ascii htype) ""
      ++ String (Char.unicode_of_ascii hlen) ""
      ++ String (Char.unicode_of_ascii hops) ""
      ++ String (Char.unicode_of_ascii (Uint63.to_int xid)) ""
      ++ String (Char.unicode_of_ascii (Uint63.to_int secs)) ""
      ++ String (Char.unicode_of_ascii (Uint63.to_int flags)) ""
      ++ ciaddr ++ yiaddr ++ siaddr ++ giaddr ++ chaddr ++ sname ++ file ++ bop
  end.

Definition update_condata :=
  fun
    (condata:list (ProtocolDefinitions.sockaddr * int))
    (conns:list ProtocolDefinitions.file_descr)
    (sad:ProtocolDefinitions.sockaddr)
    (dat:string) =>
    let pcnt := parse_content dat in
    match pcnt with
    | BOOTPRequest => (SError "BOOTPRequest not expected during communication", Nothing)
    | BOOTPReply fields => 
        let ucondata := remove_condata condata sad in
        ((ucondata, conns), OPacket (bootp_reply_to_string (BOOTPReply fields)))
    end.
    
(** Generate reply *)
Definition communication_srvr_reply
      (pd:SProtoData) (inp:SPInput) :=
  let (co, scon) := pd in
  let (co1, saddr) := co in
  let (condata, conns) := co1 in
  match inp with
  | SIPacket sad rdata =>
      let (cc, data) := update_condata condata conns sad rdata in
      let (ncondata, nconns) := cc in
      let pst := SFComm (((ncondata, nconns), saddr), scon) false in
      (pst, OPacket data)
  | SPIError msg => (SSError msg, Nothing)
  | SNothing => (SSError "Empty input data", Nothing)
  end.

Definition proto_srvr_next (st : PSState) (inp:SPInput)  : (PSState * POutput) :=
  match st with
  | SNoOp conn saddr => (** initialization of the communication *)
      (match inp with
       | SIPacket addr content =>
           communication_srvr_reply ([(addr,1%uint63)],[], saddr, conn) inp
       | SPIError msg => (SSError msg, Nothing)
       | SNothing => (SSError "Initialization is elsewhere", Nothing)
       end)
  | SFComm pd tm => (** normal communication *)
      communication_srvr_reply pd inp
  | SFinal => (* We assume that the decision to finalize is on the side of
                the protocol step, so this case should not take place  *)
     (SSError "Wrong protocol handling", Nothing) 
  | SSError msg => (** error *)
      (* Nothing can be done here. *)
      (SSError msg, Nothing)
  end.
