param(
  [string]$upstreamVersion = "continuous",
  [string]$exeVersion = "0.0.1",
  [string]$signtool = "signtool sign /sha1 FF52240075AD7D14AF25629FDF69635357C7D14B /tr http://timestamp.digicert.com /fd sha256 /d `$qCryptomator`$q `$f"
)

$buildDir = Split-Path -Parent $PSCommandPath

Write-Output "`$upstreamVersion=$upstreamVersion"
Write-Output "`$exeVersion=$exeVersion"
Write-Output "`$signtool=$signtool"
Write-Output "`$buildDir=$buildDir"
Write-Output "`$Env:JAVA_HOME=$Env:JAVA_HOME"

# determine dokan installer version
$dokanInstallerVersion = & $PSScriptRoot"\Get-MSIFileInformation.ps1" .\resources\innosetup\Dokan_x64.msi "ProductVersion"
Write-Output "`$dokanInstallerVersion=$dokanInstallerVersion"

# determine revision number
$commitInfo = Invoke-WebRequest -Method Head "https://api.github.com/repos/cryptomator/cryptomator/commits?per_page=1&sha=${upstreamVersion}"
$regex = $commitInfo.Headers.Link | Select-String -Pattern 'page=(\d{4,})'
$upstreamRevision = $regex.Matches.Groups[1].Value
$installerRevision = & git rev-list --count HEAD
$revision = "${upstreamRevision}.${installerRevision}"
Write-Output "`$revision=${revision}"

if (-not (Test-Path $Env:JAVA_HOME)) {
    Write-Output "JAVA_HOME not set or does not exist: $Env:JAVA_HOME"
    exit 1;
}

if(-not ($dokanInstallerVersion -eq [System.Diagnostics.FileVersionInfo]::GetVersionInfo(".\resources\app\dlls\dokan1.dll").FileVersion)){
    Write-Output "The Dokany version of the installer does not match the Dokan-DLL. Please update one of them such they belong to the same release."
    exit 1;
}

# cleanup
Remove-Item -Recurse -ErrorAction Ignore -Force buildkit.zip, app, libs, runtimeImage

# configure stuff
$ProgressPreference = 'SilentlyContinue' # disables Invoke-WebRequest's progress bar, which slows down downloads to a few bytes/s

# download and extract buildkit
$buildkitUrl = "https://github.com/cryptomator/cryptomator/releases/download/${upstreamVersion}/buildkit-win.zip"
Write-Output "Downloading ${buildkitUrl}..."
Invoke-WebRequest $buildkitUrl -OutFile "buildkit.zip"
Expand-Archive -Path buildkit.zip -DestinationPath . -Force
if (-not (Test-Path libs)) {
    Write-Output "libs/ does not exist. Buildkit damaged?"
    exit 1;
}

# create runtime image
& "$Env:JAVA_HOME\bin\jlink" `
  --verbose `
  --output runtimeImage `
  --module-path `"$Env:JAVA_HOME\jmods`" `
  --add-modules java.base,java.logging,java.xml,java.sql,java.management,java.security.sasl,java.naming,java.datatransfer,java.security.jgss,java.rmi,java.scripting,java.prefs,java.desktop,jdk.unsupported,java.net.http,jdk.crypto.ec `
  --no-header-files `
  --no-man-pages `
  --strip-debug `
  --strip-native-commands `
  --compress=1

# create application dir
Write-Output "Create application dir: ..."
& "$Env:JAVA_HOME\bin\jpackage" `
  --verbose `
  --type app-image `
  --runtime-image runtimeImage `
  --input libs `
  --dest app `
  --name Cryptomator `
  --vendor "Skymatic GmbH" `
  --copyright "(C) 2016 - 2021 Skymatic GmbH" `
  --app-version "${exeVersion}.${revision}" `
  --icon resources/app/Cryptomator.ico `
  --java-options "-Dfile.encoding=`"utf-8`"" `
  --java-options "-Dcryptomator.logDir=`"~/AppData/Roaming/Cryptomator`"" `
  --java-options "-Dcryptomator.settingsPath=`"~/AppData/Roaming/Cryptomator/settings.json`"" `
  --java-options "-Dcryptomator.ipcPortPath=`"~/AppData/Roaming/Cryptomator/ipcPort.bin`"" `
  --java-options "-Dcryptomator.keychainPath=`"~/AppData/Roaming/Cryptomator/keychain.json`"" `
  --java-options "-Dcryptomator.mountPointsDir=`"~/Cryptomator`"" `
  --java-options "-Dcryptomator.buildNumber=`"exe-${revision}`"" `
  --java-options "-Xss2m" `
  --java-options "-Xmx512m" `
  --main-class org.cryptomator.launcher.Cryptomator `
  --main-jar launcher-$upstreamVersion.jar

# adjust .app
& 'attrib' -r 'app/Cryptomator/Cryptomator.exe'
Copy-Item resources/app/dlls/* app/Cryptomator/

# build installer
Copy-Item -Recurse resources/innosetup/* app/
Set-Location app/
$env:CRYPTOMATOR_VERSION = "$upstreamVersion"
$env:DOKAN_VERSION = "$dokanInstallerVersion"
& 'C:\Program Files (x86)\Inno Setup 6\ISCC.exe' setup.iss /Qp "/sdefault=`"$signtool`""
