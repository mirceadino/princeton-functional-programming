all: expressiontop expression mapreduce
run: expressiontop run_exp_top

mapreduce: mapreduce.ml
	ocamlc -g -o mapreduce mapreduce.ml

LIB_FILES=ast.ml expressionLibrary.ml 
FILES=$(LIB_FILES) expression.ml

expression: $(FILES)
	ocamlc -g -o expression $(FILES)

expressiontop : $(FILES)
	ocamlmktop -o expressiontop $(FILES)

run_exp_top:
	./expressiontop

clean: 
	rm -f expression expressiontop mapreduce *.cmi *.cmo
