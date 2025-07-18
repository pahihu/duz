#!/bin/bash
cp fptst.dek tmp.$$
echo "###" >> tmp.$$
cat punch >> tmp.$$
apawk -f mkpair.awk tmp.$$ > report.log
rm -f tmp.$$
