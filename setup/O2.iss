#define AppName "O2"
#define AppVersion GetFileProductVersion("..\src\O2\O2.exe")
#define Copyright "(C) 2004-2025 Maurizio Basaglia. All rights reserved."

#define SetupDir "setup"
#define AppExeDir "src\O2"
#define LauncherDir "src\Launcher"
#define LicenseDir "setup"
#define ReadmeDir "setup"
#define ExamplesDir "setup"
#define StylesDir "src\O2\Styles"

#define AppExeFile "o2.exe"
#define LauncherFile "O2Portable.exe"
#define LicenseFile "License.rtf"
#define ReadmeFile "ReadMe.rtf"
#define WebView2LoaderFile "WebView2Loader.dll"

[Setup]
SourceDir=..
OutputBaseFilename={#AppName}-{#AppVersion}-setup
OutputDir=setup\output
DefaultDirName={commonpf}\{#AppName}
DefaultGroupName={#AppName}
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppCopyright={#Copyright}
VersionInfoVersion={#AppVersion}
VersionInfoCopyright={#Copyright}
LicenseFile={#LicenseDir}\{#LicenseFile}
InfoBeforeFile={#ReadmeDir}\{#ReadmeFile}
WizardSmallImageFile={#SetupDir}\WizardSmallImage.bmp
ChangesAssociations=true
WizardStyle=modern
UninstallDisplayIcon={app}\{#AppExeFile},0

[Types]
Name: full; Description: Full installation
Name: compact; Description: Compact installation
Name: custom; Description: Custom installation; Flags: iscustom

[Components]
Name: program; Description: {#AppName}; Types: full compact custom; Flags: fixed
Name: examples; Description: Examples; Types: full
Name: styles; Description: Styles; Types: full

[Files]
Source: {#AppExeDir}\{#AppExeFile}; DestDir: {app}; Components: program; Flags: replacesameversion
Source: {#AppExeDir}\{#WebView2LoaderFile}; DestDir: {app}; Components: program; Flags: replacesameversion
Source: {#LauncherDir}\{#LauncherFile}; DestDir: {app}; Components: program; Flags: replacesameversion
Source: {#SetupDir}\appicon.ico; DestDir: {app}; Components: program;
Source: {#SetupDir}\appicon_16.png; DestDir: {app}; Components: program;
Source: {#SetupDir}\appicon_32.png; DestDir: {app}; Components: program;
Source: {#SetupDir}\o2.ico; DestDir: {app}; Components: program;
Source: {#SetupDir}\o2_16.png; DestDir: {app}; Components: program;
Source: {#SetupDir}\o2_32.png; DestDir: {app}; Components: program;
Source: {#SetupDir}\help.html; DestDir: {app}; Components: program
Source: {#LicenseDir}\{#LicenseFile}; DestDir: {app}; Components: program
Source: {#ReadmeDir}\{#ReadmeFile}; DestDir: {app}; Components: program
Source: {#ExamplesDir}\AddressBook.o2; DestDir: {app}\Examples; Components: examples
Source: {#ExamplesDir}\PasswordWallet.o2; DestDir: {app}\Examples; Components: examples
Source: {#StylesDir}\Blocks.css; DestDir: {app}\Styles; Components: styles
Source: {#StylesDir}\Mono.css; DestDir: {app}\Styles; Components: styles
Source: {#StylesDir}\Sticky Notes.css; DestDir: {app}\Styles; Components: styles

[Icons]
Name: {group}\{#AppName}; Filename: {app}\{#AppExeFile}; WorkingDir: {app}; IconFilename: {app}\{#AppExeFile}; IconIndex: 0; Comment: Start {#AppName}
Name: {group}\License; Filename: {app}\{#LicenseFile}; Comment: View the Mozilla Public License Version 2.0
Name: {group}\Uninstall; Filename: {uninstallexe}; Comment: Uninstall {#AppName}
Name: {commondesktop}\{#AppName}; Filename: {app}\{#AppExeFile}; WorkingDir: {app}; IconFilename: {app}\{#AppExeFile}; IconIndex: 0; Comment: Start {#AppName}; Tasks: desktopicon

[Registry]
Root: HKCR; Subkey: o2project.o2; ValueType: string; ValueData: {#AppName} file; Flags: uninsdeletekey
Root: HKCR; Subkey: o2project.o2\DefaultIcon; ValueType: string; ValueData: {app}\{#AppExeFile},1
Root: HKCR; Subkey: o2project.o2\shell\open\command; ValueType: string; ValueData: """{app}\{#AppExeFile}"" ""%1"""
Root: HKCR; Subkey: .o2; ValueType: string; ValueData: o2project.o2; Flags: uninsdeletekey
Root: HKCR; Subkey: .o2; ValueType: string; ValueName: "Content Type"; ValueData: application/o2project.o2
Root: HKCR; Subkey: .o2\DefaultIcon; ValueType: none; Flags: deletekey
Root: HKCR; Subkey: .o2\shell\open; ValueType: none; Flags: deletekey
Root: HKCU; Subkey: SOFTWARE\Embarcadero\Locales; ValueType: none; ValueName: "{app}\{#AppExeFile}"; Flags: deletevalue

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}
Name: launchprogram; Description: {cm:LaunchProgram,{#AppName}}

[Run]
Filename: {app}\{#AppExeFile}; WorkingDir: {app}; Tasks: launchprogram; Flags: nowait

[InstallDelete]
Name: {app}\ReadMe.txt; Type: files
Name: {app}\License.txt; Type: files
Name: {app}\MPL-1.1.txt; Type: files
Name: {app}\upd8r.exe; Type: files
Name: {app}\upd8r.xml; Type: files
Name: {app}\o2.ENU; Type: files
Name: {app}\o2.ITA; Type: files
Name: {group}\Update.lnk; Type: files
