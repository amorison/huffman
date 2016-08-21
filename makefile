GHC=ghc
MODULES=Huffman/CodeBookBuilder.hs Huffman/Types.hs

.PHONY: all clean debug

all: huffman

debug: huffman-debug

huffman: Main.hs $(MODULES)
	$(GHC) -O2 -o $@ --make $<

huffman-debug: Main.hs $(MODULES)
	$(GHC) -rtsopts -o $@ --make $<

clean:
	@rm -f *.hi *.o
	@rm -f Huffman/*.hi Huffman/*.o
