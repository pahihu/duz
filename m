#!/bin/bash
make
rm -f printer
rm -f teste.tra
./mix -t a -a example.mix
exit 0
cat cardload.dek hello.dek >reader
# cat cardload.dek primes.dek >reader
./mix -t io -g 16 -x tester >mix.log 2>&1
