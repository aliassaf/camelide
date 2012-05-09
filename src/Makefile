SOURCES = error.ml term.ml scope.ml printer.ml rule.ml type.ml parser.mly \
          lexer.mll module.ml main.ml

GENERATED = parser.mli parser.ml lexer.ml

OBJS = error.cmo term.cmo scope.cmo printer.cmo rule.cmo type.cmo parser.cmo \
       lexer.cmo module.cmo main.cmo

camelide: $(OBJS)
	ocamlcp -o camelide $(OBJS)
	
%.cmo: %.ml
	ocamlcp -c $*.ml

%.cmi: %.mli
	ocamlcp -c $*.mli

%.cmx: %.ml
	ocamlopt -c $*.ml

%.ml %.mli: %.mly
	ocamlyacc -v $*.mly

%.ml: %.mll
	ocamllex $*.mll

stat: clean
	wc -l *.ml parser.mly lexer.mll

# Clean up
clean:
	rm -f camelide
	rm -f *.cmi *.cmo *.cmx
	rm -f parser.mli parser.ml lexer.ml parser.output

# Dependencies
.depend: $(GENERATED) $(SOURCES)
	ocamldep *.mli *.ml > .depend

include .depend
