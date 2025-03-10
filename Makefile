all: deps
	cd src && make
	ln -sf src/latc_llvm latc_comp

deps: runtime

runtime:
	cd lib && make

clean:
	cd src && make clean

distclean: clean
	cd src && make distclean
	rm -f latc_comp
