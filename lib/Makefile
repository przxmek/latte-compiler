all: runtime

runtime:
	llvm-as -o a.bc runtime.ll
	llvm-link -o runtime.bc a.bc
	rm -f a.bc
