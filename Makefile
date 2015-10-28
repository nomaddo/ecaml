SOURCES=misc location longident syntax parser lexer main
OPT=ocamlfind ocamlopt
COMPFLAGS=-package batteries,sexplib.syntax -syntax camlp4o

OCAMLLEX=ocamllex
OCAMLYACC=menhir

OBJS= $(addsuffix .cmx, $(SOURCES))

prog: beforeprog $(OBJS)
	$(OPT) $(COMPFLAGS) $(OBJS) -linkall -linkpkg -o $@

beforeprog: parser.mli parser.ml lexer.ml

parser.cmx: parser.cmi
# plexer.cmx: plexer.mll

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmx:
	$(OPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	ocamlfind ocamldep -package sexplib.syntax -syntax camlp4o *.mli *.ml > .depend

clean::
	rm -f *.cm* *.annot *.o

include .depend
