#!/bin/bash

VERBOSE=false
LOG_FILE=test.log


GOOD_DIR=good
BAD_DIR=bad

GOOD=`find $GOOD_DIR -name '*.lat'`
BAD=`find $BAD_DIR -name '*.lat'`

function log {
  msg=$1
  echo $msg >> $LOG_FILE
}

function log_file {
    out_file=$1

    log  ""
    log  "-- output ---------------"
    cat $out_file >> $LOG_FILE
    log  "-------------------------"
    log  ""
}

log  "> Testing GOOD.."
echo "> Testing GOOD.."
for file in $GOOD;
do
  out_file=`basename $file .lat`.out
  ../latc_llvm $file >$out_file 2>&1

  if [[ $? == 0 ]]; then
    echo "  [ OK ] $file"
    log  "  [ OK ] $file"
    if [[ $VERBOSE == true ]]; then
      log_file $out_file
    fi
  else
    echo "! [FAIL] $file"
    log  "! [FAIL] $file"
    log_file $out_file
  fi
done

log  ""
log  "> Testing BAD.."
echo ""
echo "> Testing BAD.."

for file in $BAD;
do
  out_file=`basename $file .lat`.out
  ../latc_llvm $file >$out_file 2>&1

  if [[ $? != 0 ]]; then
    echo "  [ OK ] $file"
    log  "  [ OK ] $file"
    if [[ $VERBOSE == true ]]; then
      log_file $out_file
    fi
  else
    echo "! [FAIL] $file"
    log  "! [FAIL] $file"
    log_file $out_file
  fi
done

echo ""
echo "See $LOG_FILE for details."
echo ""