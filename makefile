GHC=ghc
REL=huffman
DBG=huffman-debug
MODDIR=Huffman
MODULES=$(MODDIR)/CodeBook.hs $(MODDIR)/Internal.hs $(MODDIR)/Statistics.hs
RTS=-with-rtsopts="-H128m"

.PHONY: all release debug cleanall clean

all: release debug

release: $(REL)

debug: $(DBG)

$(REL): Main.hs $(MODULES)
	$(GHC) $(RTS) -O2 -o $@ --make $<

$(DBG): Main.hs $(MODULES)
	$(GHC) $(RTS) -rtsopts -o $@ --make $<

cleanall: clean
	@rm -f $(REL) $(DBG)

clean:
	@rm -f *.hi *.o
	@rm -f $(MODDIR)/*.hi $(MODDIR)/*.o
