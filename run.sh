#!/bin/sh
cat fpdrv.mix fplib.mix end.mix >a.mix
./asml a
if [ ! -s fptst.dek ];
then
  ./mix -d -m 300
fi
cat cardload.dek a.tra fptst.dek >reader
export MIXTRACE=o
rm -f punch
./mix -g -y a.sym 2>&1 | tee a.tre
