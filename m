#!/bin/bash
make
rm -f printer
rm -f teste.tra
./mix -t o -a primes.mix 2>&1 | tee mix.log
exit 0
cat cardload.dek hello.dek >reader
# cat cardload.dek primes.dek >reader
./mix -t io -g 16 -x tester >mix.log 2>&1
