$AppExeFilePath = ".\src\O2\o2.exe"
$SetupScriptFilePath = ".\setup\O2.iss"
$VersionInfoFilePath = ".\sf_net\upd8r.xml"

# Compress the main executable file

upx --compress-icons=0 $AppExeFilePath

# Build the installer file

iscc $SetupScriptFilePath

# Generate the version information file

$VersionInfo = (Get-Item $AppExeFilePath).VersionInfo

$VersionInfoFileContent = `
@'
<?xml version="1.0" encoding="utf-8"?>
<?xml-stylesheet type="text/xsl" href="version-info.xsl" media="all"?>
<Update>
  <AppName>{0}</AppName>
  <AppVersion>
    <MajorVersion>{1}</MajorVersion>
    <MinorVersion>{2}</MinorVersion>
    <Release>{3}</Release>
    <Build>{4}</Build>
  </AppVersion>
  <DownloadURL>https://github.com/maurizuki/O2/releases/latest</DownloadURL>
</Update>
'@ -f $VersionInfo.ProductName, $VersionInfo.FileVersionRaw.Major, $VersionInfo.FileVersionRaw.Minor, $VersionInfo.FileVersionRaw.Build, $VersionInfo.FileVersionRaw.Revision

New-Item $VersionInfoFilePath -Force
Set-Content $VersionInfoFilePath $VersionInfoFileContent
