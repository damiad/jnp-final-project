#LIBPREFIX=~/.opam/default/lib
LIBPREFIX=~/.opam/4.13.1/lib
OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt
OCAMLDEP=ocamlfind ocamldep
COQC=coqc
INCLUDES= -I $(LIBPREFIX)/coq-core/kernel/ -I .# all relevant -I options here
#INCLUDES= -I .# all relevant -I options here
OCAMLFLAGS=$(INCLUDES) -rectypes -thread   # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) -rectypes # add other options for ocamlopt here
OCAMLLIBS= -linkpkg -package threads
#-package stdlib ~/.opam/default/lib/coq-core/kernel/kernel.cmxa

# prog1 should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

# The list of object files for prog1
PROG1_OBJS=client.cmo

# units: mod4 and mod5.


COQGENERATEDML=  Bool.ml Datatypes.ml Decimal.ml Hexadecimal.ml \
Nat.ml BinPosDef.ml BinPos.ml BinNat.ml BinNums.ml  Ascii.ml \
Byte.ml CarryType.ml  List0.ml Logic.ml  Number.ml  PrimInt63.ml  \
Specif.ml String0.ml  ProtocolDefinitions.ml  Protocol.ml


# The list of files generated by coq
COQGENERATED := $(COQGENERATEDML) $(COQGENERATEDML:.ml=.mli)

# The list of object files for prog2
CLIENT_OBJS := helper.cmx $(COQGENERATEDML:.ml=.cmx) protoloop.cmx  bootp_client.cmx

# The list of object files for prog2
SERVER_OBJS := helper.cmx $(COQGENERATEDML:.ml=.cmx) protoloop.cmx  bootp_server.cmx



all: bootp_client bootp_server

Protocol.ml: Protocol.vo ProtocolExtraction.v
	coqc ProtocolExtraction.v


ProtocolDefinitions.ml: ProtocolDefinitions.vo ProtocolExtraction.v
	coqc ProtocolExtraction.v

ProtocolExtraction.vo: ProtocolExtraction.v Protocol.vo

protoloop.cmx: Protocol.cmx protoloop.ml

bootp_client.cmo: protoloop.cmo ProtocolDefinitions.cmo Protocol.cmo bootp_client.ml

Protocol.cmx: Protocol.ml Protocol.cmi helper.cmx

Protocol.vo: Protocol.v ProtocolDefinitions.vo Helper.vo

Helper.vo: ProtocolDefinitions.vo

bootp_client.cmx: protoloop.cmx ProtocolDefinitions.cmx Protocol.cmx 

bootp_client: $(CLIENT_OBJS) protoloop.cmx helper.cmx ProtocolDefinitions.cmx Protocol.cmx
	$(OCAMLOPT) -o bootp_client $(OCAMLFLAGS) $(OCAMLLIBS) /home/alx/.opam/4.13.1/lib/coq-core/vm/libcoqrun_stubs.a  $(LIBPREFIX)/coq-core/kernel/uint63.cmx  $(CLIENT_OBJS)


bootp_server: $(SERVER_OBJS) protoloop.cmx helper.cmx ProtocolDefinitions.cmx Protocol.cmx
	$(OCAMLOPT) -o bootp_server $(OCAMLFLAGS) $(OCAMLLIBS) /home/alx/.opam/4.13.1/lib/coq-core/vm/libcoqrun_stubs.a  $(LIBPREFIX)/coq-core/kernel/uint63.cmx  $(SERVER_OBJS) 

Protocol.cmi: Protocol.mli ProtocolDefinitions.cmi helper.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $<

ProtocolDefinitions.cmi: ProtocolDefinitions.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx 

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<a

.ml.mli:
	$(OCAMLC) $(OCAMLFLAGS) -i $< > $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) $< 

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.SUFFIXES: .v .vo .vok .vos

recvb: recvb.c
	gcc recvb.c -o recvb

.v.vo:
	$(COQC) $<

.v.vok:
	$(COQC) $<

.v.vos:
	$(COQC) $<


# Clean up
clean:
	-rm -f bootp_client bootp_server simple
	-rm -f *.cm[iox]
	-rm -f *.o
	-rm -f *.vo[ks]
	-rm -f *.vo
	-rm -f *.glob
	-rm -f $(COQGENERATED)
	-rm -f *~

# Dependencies
depend:
	$(OCAMLDEP) -I $(LIBPREFIX)/coq-core/kernel/ -I . *.mli *.ml > .depend

#include .depend

.depend: ProtocolExtraction.vo
	$(OCAMLDEP) -native $(INCLUDES) *.mli *.ml > .depend

depend: .depend

include .depend

