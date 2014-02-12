NAME = camelide

OCAMLBUILD = ocamlbuild
OPTIONS    = -classic-display
INSTALL_PATH = /usr/local/bin/

.PHONY: native byte clean stat

native:
	$(OCAMLBUILD) $(OPTIONS) -I src main.native
	mv main.native $(NAME)

byte:
	$(OCAMLBUILD) $(OPTIONS) -I src main.byte
	mv main.byte $(NAME)

clean:
	$(OCAMLBUILD) $(OPTIONS) -clean

# Statistics
stat: clean
	wc -l src/*.ml src/*.mly src/*.mll

install:
	cp $(NAME) $(INSTALL_PATH)
