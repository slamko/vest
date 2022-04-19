CC=ocamlopt
BIN=vest
LIBS=unix.cmxa
SRC_DIR=src
UNITS=types symbols entries
SRC:=$(addsuffix .mli, $(UNITS)) $(addsuffix .ml, $(UNITS) main)

vpath %.mli $(SRC_DIR)
vpath %.ml $(SRC_DIR)

$(BIN): $(SRC)
	$(CC) -I $(SRC_DIR) -o $@ $(LIBS) $^

.PHONY: clean
clean: 
	rm -f $(addprefix $(SRC_DIR)/, *.o *.cmx *.cmi *.out *.cmxa) $(BIN) 
