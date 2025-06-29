#!/bin/bash
make
rm -f printer *.tra
rm -f *.log

# assemble primes2.mix, save as prime.tra
rm -f core.mem
./mix -a primes2.mix -p -c 2>&1 | tee runmix.log

# wipe out printer and core
rm -f printer

# run from core
./mix -c -s 3000 2>&1 | tee runcore.log
exit 0

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
