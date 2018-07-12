param([string]$buildVersion = "continuous")

# cleanup
Remove-Item -Recurse -ErrorAction Ignore -Force antbuild, libs, build.xml

# configure stuff
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

# download ant (if not installed)
if ((Get-Command "ant" -ErrorAction SilentlyContinue) -eq $null) {
	if (!(Test-Path "apache-ant-1.9.12")) {
		Invoke-WebRequest "http://mirror.softaculous.com/apache//ant/binaries/apache-ant-1.9.12-bin.zip" -OutFile "ant.zip"
		Expand-Archive -Path ant.zip -DestinationPath .
	}
	$env:Path += ".\apache-ant-1.9.12\bin\";
}

# download and extract ant-kit
Invoke-WebRequest "https://github.com/cryptomator/cryptomator/releases/download/$buildVersion/antkit.zip" -OutFile "antkit.zip"
Expand-Archive -Path antkit.zip -DestinationPath .


# build application directory
& 'ant' `
  '-Dantbuild.logback.configurationFile="logback.xml"' `
  '-Dantbuild.cryptomator.settingsPath="~/AppData/Roaming/Cryptomator/settings.json"' `
  '-Dantbuild.cryptomator.ipcPortPath="~/AppData/Roaming/Cryptomator/ipcPort.bin"' `
  '-Dantbuild.cryptomator.keychainPath="~/AppData/Roaming/Cryptomator/keychain.json"' `
  '-Dantbuild.dropinResourcesRoot="./resources/app"' `
  'image'

# replace jvm
Remove-Item -Recurse -Force antbuild/Cryptomator/runtime
& "$Env:JAVA_HOME/bin/jlink" `
  "--module-path=$($Env:JAVA_HOME)\\jmods\\" `
  '--compress=1' `
  '--no-header-files' `
  '--strip-debug' `
  '--no-man-pages' `
  '--strip-native-commands' `
  '--output=antbuild/Cryptomator/runtime' `
  '--add-modules=java.base,java.logging,java.xml,java.sql,java.management,java.security.sasl,java.naming,java.datatransfer,java.security.jgss,java.rmi,java.scripting,java.prefs,java.desktop,javafx.fxml,javafx.controls' `
  '--verbose'

# adjust .app
& 'attrib' -r './antbuild/Cryptomator/Cryptomator.exe'
Copy-Item resources/app/logback.xml ./antbuild/Cryptomator/app/
Copy-Item resources/app/dlls/* ./antbuild/Cryptomator/

# build installer
Copy-Item -Recurse resources/innosetup/* ./antbuild/
Set-Location ./antbuild
$env:CRYPTOMATOR_VERSION = "$buildVersion"
& 'C:\Program Files (x86)\Inno Setup 5\ISCC.exe' setup.iss '/sdefault="signtool $p"'
