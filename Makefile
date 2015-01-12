.PHONY: FORCE clean distclean

COMPILE_OPTS = -Wall -rtsopts -O2 -threaded -feager-blackholing
orbit: FORCE
		ghc $(COMPILE_OPTS) --make Bench.hs -main-is Bench -o orbit

tests:
		ghc $(COMPILE_OPTS) Tests.hs -o OrbitTests

prof: distclean
		ghc $(COMPILE_OPTS) --make Bench.hs -main-is Bench -o orbit && \
		ghc $(COMPILE_OPTS) --make Bench.hs -main-is Bench -o orbit -prof -osuf p_o

threadscope:
		ghc $(COMPILE_OPTS) -eventlog --make Bench.hs -main-is Bench -o orbit

prof-results:
		hp2ps -e8in -c orbit && \
		echo "The memory profiling graph is in orbit.ps"

clean:
		$(RM) *.swp *~ *.hi *.o *.dyn_hi *.dyn_o *.p_o *.aux *.hp *.prof *.eventlog

distclean: clean
		$(RM) orbit OrbitTests
