EXECUTABLE = camelide
MODULES    = error term scope printer rule type parser lexer module main
GENERATED  = parser.mli parser.ml lexer.ml

# Tools
OCAMLC    = ocamlc
OCAMLOPT  = ocamlopt
OCAMLDEP  = ocamldep
OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc

# Options
BFLAGS =
OFLAGS =

$(EXECUTABLE): $(MODULES:%=src/%.cmx)
	$(OCAMLOPT) $(OFLAGS) -o $(EXECUTABLE) $(MODULES:%=src/%.cmx)

$(EXECUTABLE).byte: $(MODULES:%=%.cmo)
	$(OCAMLC) $(BFLAGS) -o $(EXECUTABLE).byte $(MODULES:%=src/%.cmo)

%.cmo: %.ml
	$(OCAMLC) $(BFLAGS) -I src -c $*.ml
%.cmi: %.mli
	$(OCAMLC) $(BFLAGS) -I src -c $*.mli
%.cmx: %.ml
	$(OCAMLOPT) $(OFLAGS) -I src -c $*.ml
%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly	
%.ml: %.mll
	$(OCAMLLEX) $*.mll

# Dependencies
.depend: $(GENERATED:%=src/%)
	ocamldep -I src src/*.ml src/*.mli > .depend

# Statistics
stat: clean
	wc -l src/*.ml src/parser.mly src/lexer.mll

# Clean up
clean:
	rm -f $(EXECUTABLE)
	rm -f src/*.cm[iox] src/*.o
	rm -f $(GENERATED:%=src/%)

include .depend
