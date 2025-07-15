if not "%1"=="" set MIXTRACE=o
if not exist fptst.dek .\mix -d -m
rem copy/y fpdrv.mix+fplib.mix+end.mix a.mix
copy/y fpdrv.mix+fplib.mix+end.mix a.mix
call asml.bat a
copy/y cardload.dek+a.tra+fptst.dek reader
set MIXCONFIG=b
if exist punch del/q punch
.\mix.exe -g -y a.sym
rem >a.tre 2>&1