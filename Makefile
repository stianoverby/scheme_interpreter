CC := ghc
PACKAGE := parsec
SRC := parser

all: $(SRC)

parser: parser.hs
	$(CC) -package $(PACKAGE) -o parser parser.hs

clean:
	rm -f *.o *.hi $(SRC)