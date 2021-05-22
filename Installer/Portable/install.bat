@echo off
CHCP 1251 > NUL
for /f "usebackq tokens=1,2,*" %%B IN (`reg query "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders" /v Desktop`) do set DESKTOP=%%D
CHCP 866 > NUL
for /f "delims=" %%i IN ('echo %DESKTOP%') do set DESKTOP=%%i
CHCP 852 > NUL

ren "%APPDATA%\Samples" "%APPDATA%\Templates"
xcopy /S /I /E *.* "%APPDATA%\Laci b '\WinBox\*.*"

SET VBSFILE="%APPDATA%\Laci b '\WinBox\Shortcut.vbs"

echo Set oWS = WScript.CreateObject("WScript.Shell") > %VBSFILE%
echo sLinkFile = "%DESKTOP%\WinBox Reloaded.lnk" >> %VBSFILE%
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> %VBSFILE%
echo oLink.TargetPath = "%APPDATA%\Laci bá'\WinBox\WinBox32.exe" >> %VBSFILE%
echo oLink.Save >> %VBSFILE%
cscript %VBSFILE%
del %VBSFILE%

del "%appdata%\Laci b '\WinBox\install.bat"

echo A telep¡t‚s sikeres.
pause