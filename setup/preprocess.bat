@echo off

rem Compress the executable file with UPX (Ultimate Packer for eXecutables)
upx --compress-icons=0 ..\src\O2\o2.exe

echo.
pause
