all: latc_llvm

latc_llvm: MainLatte.hs
	# ghc $^ -o ../$@
	ghc MainLatte.hs -o ../latc_llvm

clean:
	-rm -rf *.o *.hi