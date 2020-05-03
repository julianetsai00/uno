MODULES=uno action gamestate play
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
PLAY=play.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(PLAY) && ./$(PLAY)

check:
	bash checkenv.sh && bash checktypes.sh
	
# finalcheck: check
# 	bash checkzip.sh
# 	bash finalcheck.sh

zip:
	zip uno.zip *.ml* _tags Makefile *.txt*
	
docs: docs-public
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d doc.public \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public uno.zip