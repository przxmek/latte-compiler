all: latc_llvm

latc_llvm: MainLatte.hs
	ghc $^ -o ../$@

clean:
	-rm -rf *.o *.hi
