param(
  [string]$buildVersion = "continuous",
  [string]$signtool = "signtool sign /sha1 6FDEC9DFCFE59E6BAEE64B7ED97F00E120E70D97 `$p"
)

$buildDir = Split-Path -Parent $PSCommandPath

Write-Output "`$buildVersion=$buildVersion"
Write-Output "`$signtool=$signtool"
Write-Output "`$buildDir=$buildDir"
Write-Output "`$Env:JAVA_HOME=$Env:JAVA_HOME"


if (-not (Test-Path $Env:JAVA_HOME)) {
    Write-Output "JAVA_HOME not set or does not exist: $Env:JAVA_HOME"
    exit 1;
}

if(-not (Test-Path $Env:JAVA_HOME\jmods\jdk.packager.jar)){
    Write-Output "$Env:JAVA_HOME\jmods\jdk.packager.jar does not exist. You need to patch your JDK installation with the jar provided under ./tools"
    exit 1;
}

if(-not (Test-Path $Env:JAVA_HOME\bin\jdk.packager.jar)){
    Write-Output "$Env:JAVA_HOME\bin\jdk.packager.jar does not exist. You need to patch your JDK installation with the jar provided under ./tools"
    exit 1;
}

if(-not (Test-Path $Env:JAVA_HOME\bin\jpackager.exe)){
    Write-Output "$Env:JAVA_HOME\bin\jpackager.exe does not exist. You need to patch your JDK installation with the exe provided under ./tools"
    exit 1;
}

if(-not (Test-Path "C:\Program Files (x86)\Resource Hacker\ResourceHacker.exe")){
    Write-Output "C:\Program Files (x86)\Resource Hacker\ResourceHacker.exe does not exist. Please install from http://www.angusj.com/resourcehacker."
    exit 1;
}

# cleanup
Remove-Item -Recurse -ErrorAction Ignore -Force buildkit.zip, app, libs

# configure stuff
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

# download and extract build-kit
Invoke-WebRequest "https://dl.bintray.com/cryptomator/cryptomator/buildkit-${buildVersion}.zip" -OutFile "buildkit.zip"
Expand-Archive -Path buildkit.zip -DestinationPath .

# create application dir
& "$Env:JAVA_HOME\bin\jpackager" `
  create-image `
  --verbose `
  --echo-mode `
  --input libs `
  --output app `
  --name Cryptomator `
  --class org.cryptomator.launcher.Cryptomator `
  --main-jar launcher-$buildVersion.jar `
  --icon resources/app/Cryptomator.ico `
  --jvm-args "-Dlogback.configurationFile=`"logback.xml`"" `
  --jvm-args "-Dcryptomator.settingsPath=`"~/AppData/Roaming/Cryptomator/settings.json`"" `
  --jvm-args "-Dcryptomator.ipcPortPath=`"~/AppData/Roaming/Cryptomator/ipcPort.bin`"" `
  --jvm-args "-Dcryptomator.keychainPath=`"~/AppData/Roaming/Cryptomator/keychain.json`"" `
  --jvm-args "-Xss2m" `
  --jvm-args "-Xmx512m" `
  --identifier org.cryptomator `
  --version $buildVersion `
  --module-path `"$Env:JAVA_HOME\jmods`" `
  --add-modules java.base,java.logging,java.xml,java.sql,java.management,java.security.sasl,java.naming,java.datatransfer,java.security.jgss,java.rmi,java.scripting,java.prefs,java.desktop,jdk.unsupported `
  --strip-native-commands

# adjust .app
& 'attrib' -r 'app/Cryptomator/Cryptomator.exe'
& 'C:\Program Files (x86)\Resource Hacker\ResourceHacker.exe' `
  -open "resources\resourcehacker\cryptomator.rc" `
  -save "resources\resourcehacker\cryptomator.res" `
  -action compile
& 'C:\Program Files (x86)\Resource Hacker\ResourceHacker.exe' `
  -open "app\Cryptomator\Cryptomator.exe" `
  -resource "resources\resourcehacker\cryptomator.res" `
  -save "app\Cryptomator\Cryptomator.exe" `
  -action modify `
  -mask ",,,"
Copy-Item resources/app/logback.xml app/Cryptomator/app/
Copy-Item resources/app/dlls/* app/Cryptomator/

# build installer
Copy-Item -Recurse resources/innosetup/* app/
Set-Location app/
$env:CRYPTOMATOR_VERSION = "$buildVersion"
& 'C:\Program Files (x86)\Inno Setup 5\ISCC.exe' setup.iss /Qp "/sdefault=`"$signtool`""
