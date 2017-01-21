all: latc_llvm

latc_llvm: MainLatte.hs
	ghc $^ -o ../$@

clean:
	-rm -rf *.o *.hi
	-rm -rf Frontend/*.o Frontend/*.hi
	-rm -rf Generator/*.o Generator/*.hi
