del/q printer
del/q teste.tra
copy /y cardload.dek+hello.dek reader
rem copy /y cardload.dek+primes.dek reader
.\mix.exe -t -u -g 16 -x tester >mix.log 2>&1
