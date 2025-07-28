#!/bin/bash
cp fptst.dek tmp.$$
echo "###" >> tmp.$$
cat punch >> tmp.$$
./awk_ape.exe -f mkpair.awk tmp.$$ > report.log
rm -f tmp.$$
