#define AppVersion = "2.2.1"

[Setup]
SourceDir=..
OutputBaseFilename=O2-{#AppVersion}-setup
OutputDir=setup\output
DefaultDirName={pf}\O2
DefaultGroupName=O2
AppName=O2
AppVerName=O2 {#AppVersion}
AppCopyright=© Maurizio Basaglia. All rights reserved.
VersionInfoVersion={#AppVersion}
VersionInfoCopyright=(C) 2004-2021 Maurizio Basaglia. All rights reserved.
LicenseFile=setup\License.txt
InfoBeforeFile=setup\ReadMe.txt
WizardImageFile=setup\WizardImage.bmp
WizardSmallImageFile=setup\WizardSmallImage.bmp
ChangesAssociations=true

[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: it; MessagesFile: compiler:Languages\Italian.isl

[Types]
Name: full; Description: Full installation
Name: compact; Description: Compact installation
Name: custom; Description: Custom installation; Flags: iscustom

[Components]
Name: program; Description: O2; Types: full compact custom; Flags: fixed
Name: examples; Description: Examples; Types: full
Name: languages; Description: Languages; Types: full
Name: languages\en; Description: English; Types: full compact custom; Flags: fixed
Name: languages\it; Description: Italian; Types: full

[Files]
Source: src\O2\o2.exe; DestDir: {app}; Components: program; Flags: replacesameversion
Source: src\Launcher\O2Portable.exe; DestDir: {app}; Components: program; Flags: replacesameversion
Source: setup\appicon.ico; DestDir: {app}; Components: program;
Source: setup\appicon_16.png; DestDir: {app}; Components: program;
Source: setup\appicon_32.png; DestDir: {app}; Components: program;
Source: setup\o2.ico; DestDir: {app}; Components: program;
Source: setup\o2_16.png; DestDir: {app}; Components: program;
Source: setup\o2_32.png; DestDir: {app}; Components: program;
Source: setup\help.html; DestDir: {app}; Components: program
Source: setup\License.txt; DestDir: {app}; Components: program
Source: setup\ReadMe.txt; DestDir: {app}; Components: program
Source: setup\AddressBook.o2; DestDir: {app}\Examples; Components: examples
Source: setup\PasswordWallet.o2; DestDir: {app}\Examples; Components: examples
Source: src\O2\o2.ENU; DestDir: {app}; Components: languages\en; Flags: ignoreversion
Source: src\O2\o2.ITA; DestDir: {app}; Components: languages\it; Flags: ignoreversion

[Icons]
Name: {group}\O2; Filename: {app}\o2.exe; WorkingDir: {app}; IconFilename: {app}\o2.exe; IconIndex: 0; Comment: Start O2
Name: {group}\License; Filename: {app}\License.txt; Comment: View the Mozilla Public License Version 2.0
Name: {group}\Uninstall; Filename: {uninstallexe}; Comment: Uninstall O2
Name: {commondesktop}\O2; Filename: {app}\o2.exe; WorkingDir: {app}; IconFilename: {app}\o2.exe; IconIndex: 0; Comment: Start O2; Tasks: desktopicon

[Registry]
Root: HKCR; Subkey: o2project.o2; ValueType: string; ValueData: O2 file; Flags: uninsdeletekey
Root: HKCR; Subkey: o2project.o2\DefaultIcon; ValueType: string; ValueData: {app}\o2.exe,1
Root: HKCR; Subkey: o2project.o2\shell\open\command; ValueType: string; ValueData: """{app}\o2.exe"" ""%1"""
Root: HKCR; Subkey: .o2; ValueType: string; ValueData: o2project.o2; Flags: uninsdeletekey
Root: HKCR; Subkey: .o2; ValueType: string; ValueName: "Content Type"; ValueData: application/o2project.o2
Root: HKCR; Subkey: .o2\DefaultIcon; ValueType: none; Flags: deletekey
Root: HKCR; Subkey: .o2\shell\open; ValueType: none; Flags: deletekey

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}
Name: launchprogram; Description: {cm:LaunchProgram,O2}

[Run]
Filename: {app}\o2.exe; WorkingDir: {app}; Tasks: launchprogram; Flags: nowait

[InstallDelete]
Name: {app}\MPL-1.1.txt; Type: files
Name: {app}\upd8r.exe; Type: files
Name: {app}\upd8r.xml; Type: files
Name: {group}\Update.lnk; Type: files
