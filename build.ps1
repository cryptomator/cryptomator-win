param([string]$buildVersion = "continuous")

# cleanup
Remove-Item -Recurse -Force ant libs antkit.zip build.xml

# download build dependencies
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
Invoke-WebRequest "http://mirror.softaculous.com/apache//ant/binaries/apache-ant-1.9.12-bin.zip" -OutFile "ant.zip"
Expand-Archive -Path ant.zip -DestinationPath .

# download and extract ant-kit
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
Invoke-WebRequest "https://github.com/cryptomator/cryptomator/releases/download/$buildVersion/antkit.zip" -OutFile "antkit.zip"
Expand-Archive -Path antkit.zip -DestinationPath .


# build application directory
.\apache-ant-1.9.12\bin\ant `
  '-Dantbuild.logback.configurationFile="logback.xml"' `
  '-Dantbuild.cryptomator.settingsPath="~/AppData/Roaming/Cryptomator/settings.json"' `
  '-Dantbuild.cryptomator.ipcPortPath="~/AppData/Roaming/Cryptomator/ipcPort.bin"' `
  '-Dantbuild.cryptomator.keychainPath="~/AppData/Roaming/Cryptomator/keychain.json"' `
  '-Dantbuild.dropinResourcesRoot="./resources/app"' `
  image
  
# adjust .app
& 'attrib' -r './antbuild/Cryptomator/Cryptomator.exe'
Copy-Item resources/app/logback.xml ./antbuild/Cryptomator/app

# build installer
Copy-Item -Recurse innosetup/* ./antbuild/
Set-Location ./antbuild
$env:CRYPTOMATOR_VERSION = "$buildVersion"
& 'C:\Program Files (x86)\Inno Setup 5\ISCC.exe' setup.iss '/sdefault="signtool $p"'

$host.UI.RawUI.ReadKey()