del *.dcu /s
del *.ppu /s
del *.o /s
del *.~* /s
del *.cfg /s
del *.dsk /s
del *.dof /s
del *.bk? /s
del *.mps /s
del *.rst /s
del *.vlb /s
del *.s /s
del *.a /s
echo --------------------------------------------------------
echo delete directories with subdirectories and files 
echo---------------------------------------------------------
for /r %1 %%R in (backup) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (lib) do if exist "%%R" (rd /s /q "%%R")
