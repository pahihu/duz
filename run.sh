#!/bin/sh
if [ $# -ne 0 ];
then
  trace=1
else
  trace=0
fi

cat fpdrv.mix fplib.mix end.mix >a.mix
./asml a
if [ ! -s fptst.dek ];
then
  ./mix -d -m
fi
cat cardload.dek a.tra fptst.dek >reader
if [ $trace -ne 0 ];
then
  export MIXTRACE=o
fi
rm -f punch
export MIXCONFIG=bf
./mix -w -g -y a.sym >a.tre 2>&1
# ./mix -w -g -y a.sym 2>&1 | tee a.tre
