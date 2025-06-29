mingw32-make
del/q printer
del/q teste.tra
.\mix.exe -t o testdk.mix >mix.log 2>&1
goto end
rem copy /y cardload.dek+prime.tra reader
rem copy /y cardload.dek+primes.dek reader
.\mix.exe -t io -g >mix.log 2>&1
:end

