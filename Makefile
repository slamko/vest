CC=ocamlopt
BIN=vest
LIBS=unix.cmxa
SRC_DIR=src
TYPES=types
SYMBOLS=symbols
ENTRIES=entries
MAIN=main
SRC := $(addsuffix .mli, $(TYPES) $(SYMBOLS) $(ENTRIES)) $(addsuffix .ml, $(TYPES) $(SYMBOLS) $(ENTRIES) $(MAIN))

vpath %.mli $(SRC_DIR)
vpath %.ml $(SRC_DIR)

$(BIN): $(SRC)
	$(CC) -I $(SRC_DIR) -o $@ $(LIBS) $^

.PHONY: clean
clean: 
	rm -f $(addprefix $(SRC_DIR)/, *.o *.cmx *.cmi *.out *.cmxa) $(BIN) 
