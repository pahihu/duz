if not "%1"=="" set MIXTRACE=o
if not exist fptst.dek .\mix -d -m
rem copy/y fpdrv.mix+fplib.mix+end.mix a.mix
copy/y fpdrv.mix+fplib.mix+end.mix a.mix
call asml.bat a
copy/y cardload.dek+a.tra+fptst.dek reader
rem set MIXTRACE=o
if exist punch del/q punch
.\mix.exe -g -y a.sym >a.tre 2>&1 