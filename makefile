# "GITI tool" ; hxa7241 ; 2017 / OCaml / software .

# dependencies: (removed)
# * ocaml-re -- https://github.com/ocaml/ocaml-re


EXE=giti-o
SRCPATH=src/
SRC=$(SRCPATH)hxaGeneral.mli $(SRCPATH)hxaGeneral.ml \
    $(SRCPATH)basics.mli     $(SRCPATH)basics.ml \
    $(SRCPATH)time.mli       $(SRCPATH)time.ml \
    $(SRCPATH)pitch.mli      $(SRCPATH)pitch.ml \
    $(SRCPATH)act.mli        $(SRCPATH)act.ml \
    $(SRCPATH)sound.mli      $(SRCPATH)sound.ml \
    $(SRCPATH)indicator.mli  $(SRCPATH)indicator.ml \
    $(SRCPATH)annotation.mli $(SRCPATH)annotation.ml \
    $(SRCPATH)piece.mli      $(SRCPATH)piece.ml \
    $(SRCPATH)print.mli      $(SRCPATH)print.ml \
    $(SRCPATH)giti.ml
#LIBSF=re
#LIBS=unix.cmxa Str.cmxa re_perl.cmxa
LIBS=unix.cmxa Str.cmxa
OPTS=-principal -strict-sequence -safe-string -strict-formats -nodynlink -w +A
# -noassert -unsafe -O3
OPTS2=-I src/


all: exes

exes: $(EXE)
$(EXE): $(SRC)
	ocamlopt.opt -o $(EXE) $(OPTS) $(OPTS2) $(LIBS) $(SRC)
#	ocamlfind ocamlopt -package $(LIBSF) -linkpkg -o $(EXE) $(OPTS) \
      $(OPTS2) $(LIBS) $(SRC)
	rm -f $(SRCPATH)*.cm[ixo] $(SRCPATH)*.o


.PHONY: clean
clean:
	rm -f $(SRCPATH)*.cm[ixoa] $(SRCPATH)*.cmxa $(SRCPATH)*.[ao]
	rm -f $(EXE)
