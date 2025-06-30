@echo off
echo:
echo *** %date%-%time% ASML TASK STARTED ***************

if "%1"=="" goto usage

echo:
echo ===============================================================
echo ASSEMBLY LISTING: %1.prn
echo       TRANS DECK: %1.tra
echo     TRACE OUTPUT: %1.tre
echo      PRINTER LOG: %1.log
echo ===============================================================
echo:

if exist printer del/q printer
if exist %1.tra del/q %1.tra

.\mix.exe -a %1.mix -p >%1.prn 2>&1

copy/y cardload.dek+%1.tra reader

if "%MIXTRACE%"=="" goto :notrace
.\mix.exe -t %MIXTRACE% -g > %1.tre 2>&1
goto printer

:notrace
.\mix.exe -g > %1.tre 2>&1

:printer
if exist "printer" copy/y printer %1.log
goto end

:usage
echo:
echo USAGE: asmlg source[.mix]

:end
echo:
echo *** %date%-%time% ASML TASK ENDED *****************
echo:

