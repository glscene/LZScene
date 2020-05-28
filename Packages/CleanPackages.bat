del *.res /s
del *.ico /s
del *.dcu /s
del *.local /s
del *.identcache /s
del *.obj /s
del *.~* /s
del *.dsk /s
del *.ddp /s
del *.log /s
del *.tvsconfig /s


echo --------------------------------------------------------
echo delete directories with subdirectories and files 
echo---------------------------------------------------------
for /r %1 %%R in (backup) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (lib) do if exist "%%R" (rd /s /q "%%R")
