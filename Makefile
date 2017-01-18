# all: cabal # Cabal build
all:
	cd src && $(MAKE)

# Cabal build
# cabal:
# 	cabal configure && cabal build
#	  ln -sf dist/build/latte-compiler/latte-compiler latc_llvm


# cabal-clean:
# 	cabal clean

# clean: cabal-clean # Cabal build
clean:
	cd src && $(MAKE) clean

distclean: clean
	-rm latc_llvm
