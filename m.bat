mingw32-make
del/q printer
del/q *.log
del/q *.tra
del/q core.mem
.\mix.exe -a primes2.mix -p -c >runmix.log 2>&1
.\mix.exe -c -t io -s 3000 > runcore.log 2>&1
goto end
rem copy /y cardload.dek+prime.tra reader
rem copy /y cardload.dek+primes.dek reader
.\mix.exe -t io -g >mix.log 2>&1
:end

