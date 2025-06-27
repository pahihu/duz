#!/bin/bash
make
rm -f printer
rm -f *.tra
# ./mix -a primes2.mix -x prime 2>&1 | tee mix.log
./mix -t o -a testrs.mix 2>&1 | tee mix.log
exit 0
cat cardload.dek hello.dek >reader
# cat cardload.dek primes.dek >reader
./mix -t io -g 16 -x tester >mix.log 2>&1
