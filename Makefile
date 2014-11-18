.PHONY: FORCE clean distclean

orbit: FORCE
		ghc --make Bench.hs -o orbit

clean:
		$(RM) *.swp *~ *.hi *.o

distclean: clean
		$(RM) orbit
