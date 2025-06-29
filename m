#!/bin/bash
make
rm -f printer *.tra

# assemble primes2.mix, save as prime.tra
./mix -a primes2.mix -p 2>&1 | tee runmix.log

# wipe out printer
rm -f printer

# load cardload and prime.tra into card reader
cat cardload.dek prime.tra >reader

# push the GO button on the card reader
./mix -g 2>&1 | tee runprime.log

# ./mix -t o taocp.mix 2>&1 | tee mix.log
# ./mix -t o no1.mix 2>&1 | tee mix.log
# ./mix -t o maximum.mix 2>&1 | tee mix.log
exit 0
cat cardload.dek hello.dek >reader
# cat cardload.dek primes.dek >reader
./mix -t io -g -p >mix.log 2>&1
