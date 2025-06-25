mingw32-make
del/q printer
del/q teste.tra
.\mix.exe -a example.mix >mix.log 2>&1
goto end
copy /y cardload.dek+hello.dek reader
rem copy /y cardload.dek+primes.dek reader
.\mix.exe -t io -g 16 -x tester >mix.log 2>&1
:end

