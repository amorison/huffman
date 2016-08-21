GHC=ghc
MODULES=Huffman/CodeBookBuilder.hs Huffman/Types.hs

.PHONY: all clean

all: huffman

huffman: Main.hs $(MODULES)
	$(GHC) -O2 -o $@ --make $<

clean:
	@rm -f *.hi *.o
	@rm -f Huffman/*.hi Huffman/*.o
