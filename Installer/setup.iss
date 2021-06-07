; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "WinBox Reloaded"
#define MyAppVersion "1.1"
#define MyAppPublisher "Laci b�'"
#define MyAppURL "https://users.atw.hu/laciba/"    

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{649CA9A4-0D9A-49E5-ABDE-04658514CB8B}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\Laci b�'\{#MyAppName}     
LicenseFile="LICENSE.txt"
AllowNoIcons=yes
DisableProgramGroupPage=yes
; Uncomment the following line to run in non administrative install mode (install for current user only.)
;PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
OutputBaseFilename=mysetup
Compression=lzma2/ultra64
SetupIconFile=setup.ico
WizardImageFile=banner.bmp        
WizardSmallImageFile=logo.bmp
WizardImageStretch=true
SolidCompression=yes
WizardStyle=modern
VersionInfoVersion=1.12.0.0
ArchitecturesInstallIn64BitMode=x64

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "hungarian"; MessagesFile: "compiler:Languages\Hungarian.isl"          

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[InstallDelete]                        
Type: filesandordirs; Name: "{app}\Samples"
Type: filesandordirs; Name: "{app}\Templates"     

[Files]
Source: "Win64\Release\WinBox32.exe"; DestDir: "{app}"; DestName: "WinBox64.exe"; Flags: ignoreversion; Check: Is64BitInstallMode       
Source: "Win64\Release\libWinBox.dll"; DestDir: "{app}"; Flags: ignoreversion; Check: Is64BitInstallMode          
Source: "Win32\Release\WinBox32.exe"; DestDir: "{app}"; Flags: ignoreversion; Check: not Is64BitInstallMode       
Source: "Win32\Release\libWinBox.dll"; DestDir: "{app}"; Flags: ignoreversion; Check: not Is64BitInstallMode   
Source: "hu-HU\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs;       
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\WinBox64.exe"; Check: Is64BitInstallMode   
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\WinBox64.exe"; Tasks: desktopicon; Check: Is64BitInstallMode   
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\WinBox32.exe"; Check: not Is64BitInstallMode   
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\WinBox32.exe"; Tasks: desktopicon; Check: not Is64BitInstallMode   

[Run]
Filename: "{app}\WinBox32.exe"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent; Check: not Is64BitInstallMode    
Filename: "{app}\WinBox64.exe"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent; Check: Is64BitInstallMode 

