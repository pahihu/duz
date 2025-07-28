echo "###" > sep.tmp
copy/y fptst.dek+sep.tmp+punch inp.tmp
./awk_ape.exe -f mkpair.awk inp.tmp > report.log
del/q inp.tmp sep.tmp
