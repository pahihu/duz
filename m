#!/bin/bash
cat cardload.dek hello.dek >reader
# cat cardload.dek primes.dek >reader
./mix -t -u -g 16 -x tester >mix.log 2>&1
