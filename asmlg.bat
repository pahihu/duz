@echo off
echo:
echo *** %date%-%time% ASML TASK STARTED ***************

if "%1"=="" goto usage

echo:
echo ===============================================================
echo     TRACE OUTPUT: %1.tre
echo      PRINTER LOG: %1.log
echo ===============================================================
echo:

if exist %1.tre  del/q %1.tre
if exist %1.log  del/q %1.log
if exist reader  del/q reader
if exist printer del/q printer

call asml.bat %1

copy/y cardload.dek+%1.tra reader

.\mix.exe -g > %1.tre 2>&1

if exist "printer" copy/y printer %1.log
goto end

:usage
echo:
echo USAGE: asmlg source[.mix]

:end
echo:
echo *** %date%-%time% ASML TASK ENDED *****************
echo:

