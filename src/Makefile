all: latc_llvm

latc_llvm: MainLatte.hs
	ghc --make $^ -o $@

clean:
	-rm -rf *.o *.hi
	-rm -rf Frontend/*.o Frontend/*.hi
	-rm -rf Generator/*.o Generator/*.hi

distclean: clean
	rm -f latc_llvm
