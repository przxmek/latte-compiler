all: latc_llvm

latc_llvm: MainLatte.hs
	ghc --make $^ -o $@

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

distclean: clean
	rm -f latc_llvm
