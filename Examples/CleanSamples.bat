echo off
del *.exe /s  
del *.scr /s
del *.ppu /s 
del *.ldp /s
del *.ppu /s
del *.o /s
del *.~* /s
del *.log /s
del *.dsk /s
del *.dof /s
del *.bk? /s
del *.mps /s
del *.rst /s
del *.s /s
del *.a /s
del *.map /s
del *.rsm /s
del *.drc /s
del *.2007 /s
del *.local /s
del *.dproj /s

rem delete more files

del *.bak /s
del *.dbg /s
del *.xml /s
del *.identcache /s
del *.otares /s
del *.tvsconfig /s
del *.db /s


echo _
echo ************************************************
echo             Don't delete some files
echo ************************************************
echo _

rem del *.res /s
rem del *.ico /s  - some projects have own icos
rem del *.obj /s  - obj models and resources for lazarus
rem del *.cfg /s  - there are quake's animations


echo --------------------------------------------------------
echo delete directories with subdirectories and files 
echo---------------------------------------------------------
for /r %1 %%R in (backup) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (lib) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (bin) do if exist "%%R" (rd /s /q "%%R")
