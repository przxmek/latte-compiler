#!/bin/bash

FILES=`find ./test/good/ -name \*.lat | sort`

for file in $FILES; do
  out="`echo $file | sed s/lat/output/g`" && \
  echo $file && ./src/latc_llvm $file > prog.ll && \
  llvm-as -o tmp_file.bc prog.ll && \
  llvm-link -o prog_out.bc tmp_file.bc ./lib/runtime.bc && \
  input="`echo $file | sed s/lat/input/g`" && \
  if [ -f $input ]; then
    diff -bsq $out <(lli prog_out.bc < $input)
  else
    diff -bsq $out <(lli prog_out.bc)
  fi
done
