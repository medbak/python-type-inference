all: run

run: lexer.cmo parser.cmo p.cmo error.cmo main.cmo
	ocamlc -o run p.cmo error.cmo lexer.cmo parser.cmo main.cmo

error.cmo : error.ml
	ocamlc -c error.ml

p.cmo : p.ml
	ocamlc -c p.ml

parser.ml: parser.mly p.cmo
	ocamlyacc -v parser.mly

parser.mli: parser.mly
	ocamlyacc -v parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo :  main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml error.cmo
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
