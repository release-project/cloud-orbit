.PHONY: FORCE clean distclean

orbit: FORCE
		ghc -Wall --make Bench.hs -main-is Bench -o orbit

tests: orbit
	        ghc -package test-framework -package test-framework-hunit \
			-threaded Tests.hs -o OrbitTests

clean:
		$(RM) *.swp *~ *.hi *.o

distclean: clean
		$(RM) orbit OrbitTests
