#!/bin/bash

FILES=`find ./test/good/ -name \*.lat | sort`

TMP=/tmp/latc_llvm_334685
mkdir -p $TMP

for file in $FILES; do
  out="`echo $file | sed s/lat/output/g`" && \
  echo $file && ./src/latc_llvm $file > $TMP/prog.ll && \
  llvm-as -o $TMP/tmp_file.bc $TMP/prog.ll && \
  llvm-link -o $TMP/prog_out.bc $TMP/tmp_file.bc ./lib/runtime.bc && \
  input="`echo $file | sed s/lat/input/g`" && \
  if [ -f $input ]; then
    diff -bsq $out <(lli $TMP/prog_out.bc < $input)
  else
    diff -bsq $out <(lli $TMP/prog_out.bc)
  fi
done
