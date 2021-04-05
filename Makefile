MODULES=board cell hexMap plant player raster store plantInventory playerId
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test_hexmap.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private