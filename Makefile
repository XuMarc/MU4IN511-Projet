OCAMLC = ocamlc
SRC = test.ml
OBJ = $(SRC:.ml=.cmo)
EXE = exec

all: $(EXE)

$(EXE): $(OBJ)
	$(OCAMLC) -o $@ $^

%.cmo: %.ml
	$(OCAMLC) -c $<

.PHONY: clean
clean:
	rm -f $(OBJ) $(SRC:.ml=.cmi) $(EXE)
