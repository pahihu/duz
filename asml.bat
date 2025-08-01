@echo off
echo:
echo *** %date%-%time% ASML TASK STARTED ***************
echo:

if "%1"=="" goto usage

echo ===============================================================
echo ASSEMBLY LISTING: %1.prn
echo       TRANS DECK: %1.tra
echo ===============================================================
echo:

if exist %1.prn  del/q %1.prn
if exist %1.tra  del/q %1.tra
if exist %1.sym  del/q %1.tra

.\mix_ape.exe -a %1.mix -y %1.sym -p >%1.prn 2>&1
goto end

:usage
echo USAGE: asml source[.mix]

:end
echo:
echo *** %date%-%time% ASML TASK ENDED *****************
echo:
