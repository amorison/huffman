GHC=ghc
REL=huffman
DBG=huffman-debug
MODDIR=Huffman
MODULES=$(MODDIR)/CodeBook.hs $(MODDIR)/Internal.hs $(MODDIR)/Statistics.hs

.PHONY: all release debug cleanall clean

all: release debug

release: $(REL)

debug: $(DBG)

$(REL): Main.hs $(MODULES)
	$(GHC) -O2 -o $@ --make $<

$(DBG): Main.hs $(MODULES)
	$(GHC) -rtsopts -o $@ --make $<

cleanall: clean
	@rm -f $(REL) $(DBG)

clean:
	@rm -f *.hi *.o
	@rm -f $(MODDIR)/*.hi $(MODDIR)/*.o
