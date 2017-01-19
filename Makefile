all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY: main.native
main.native:
	ocamlbuild -Is util,x86,grading -libs unix,str,nums main.native -use-menhir

.PHONY: clean
clean:
	ocamlbuild -clean
