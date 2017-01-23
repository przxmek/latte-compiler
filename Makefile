all:
	cd src && $(MAKE)
	ln -sf src/latc_llvm latc_llvm


clean:
	cd src && $(MAKE) clean

distclean: clean
	cd src && $(MAKE) distclean
	rm -f latc_llvm
