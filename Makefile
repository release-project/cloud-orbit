.PHONY: FORCE clean distclean

COMPILE_OPTS = -Wall -rtsopts -O2 -threaded

orbit: FORCE
		ghc $(COMPILE_OPTS) --make Bench.hs -main-is Bench -o orbit

tests:
		ghc $(COMPILE_OPTS) Tests.hs -o OrbitTests

clean:
		$(RM) *.swp *~ *.hi *.o *.dyn_hi *.dyn_o

distclean: clean
		$(RM) orbit OrbitTests
