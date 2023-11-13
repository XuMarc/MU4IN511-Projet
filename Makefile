OCAMLC = ocamlc
SRC = projet.ml
OBJ = $(SRC:.ml=.cmo)
EXE = projet

all: $(EXE)

$(EXE): $(OBJ)
	$(OCAMLC) -o $@ $^

%.cmo: %.ml
	$(OCAMLC) -c $<

.PHONY: clean
clean:
	rm -f $(OBJ) $(SRC:.ml=.cmi) $(EXE)
