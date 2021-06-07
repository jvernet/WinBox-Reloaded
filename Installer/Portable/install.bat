@echo off
net session >nul 2>&1
if %errorLevel% == 0 goto ErrorAdmin 

echo.
echo      WinBox Reloaded by Laci ba'
echo      Version: v1.1
echo. 
echo  The program will install WinBox Reloaded to "%APPDATA%\Laci b '\WinBox\"
echo  To start the process press Enter.
echo.
pause

reg Query "HKLM\Hardware\Description\System\CentralProcessor\0" | find /i "x86" > NUL && set OS=Win32 || set OS=Win64

CHCP 1251 > NUL
for /f "usebackq tokens=1,2,*" %%B IN (`reg query "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders" /v Desktop`) do set DESKTOP=%%D
CHCP 866 > NUL
for /f "delims=" %%i IN ('echo %DESKTOP%') do set DESKTOP=%%i
CHCP 852 > NUL

@echo on

prompt $g
ren "%APPDATA%\Samples" "%APPDATA%\Templates"
del "%APPDATA%\Laci b '\WinBox\*.*" /F /Q
xcopy /S /I /E *.* "%APPDATA%\Laci b '\WinBox\*.*" /Y

@echo off

SET VBSFILE="%APPDATA%\Laci b '\WinBox\Shortcut.vbs"

echo Set oWS = WScript.CreateObject("WScript.Shell") > %VBSFILE%
echo sLinkFile = "%DESKTOP%\WinBox Reloaded.lnk" >> %VBSFILE%
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> %VBSFILE%
echo oLink.TargetPath = "%APPDATA%\Laci bá'\WinBox\%OS%\WinBox32.exe" >> %VBSFILE%
echo oLink.Save >> %VBSFILE%
cscript %VBSFILE%
del %VBSFILE%

del "%appdata%\Laci b '\WinBox\install.bat"

echo Installation is succsessful.
goto End

:ErrorAdmin
echo Don't run this program with admin rights.

:End
prompt $p$g
pause