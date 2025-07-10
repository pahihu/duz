#!/bin/sh
cat fpdrv.mix fplib.mix end.mix >a.mix
./asml a
cat cardload.dek a.tra fptst.dek >reader
# export MIXTRACE=o
rm -f punch
./mix -g 2>&1 | tee a.tre
