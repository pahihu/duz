echo "###" > sep.tmp
copy/y fptst.dek+sep.tmp+punch inp.tmp
nawk -f mkpair.awk inp.tmp > report.log
del/q inp.tmp sep.tmp
