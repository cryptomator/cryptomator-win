;This file will be executed next to the application bundle image
;I.e. current directory will contain folder Cryptomator with application files
[Setup]
#define AppVersion GetEnv("CRYPTOMATOR_VERSION")
#define FileInfoVersion GetFileVersion("Cryptomator/Cryptomator.exe")

SignTool=default /tr http://timestamp.comodoca.com /fd sha256 /d $qCryptomator$q $f
AppId={{Cryptomator}}
AppName=Cryptomator
AppVersion={#AppVersion}
AppPublisher=cryptomator.org
AppCopyright=cryptomator.org
AppPublisherURL=https://cryptomator.org/
AppSupportURL=https://cryptomator.org/help/
AppUpdatesURL=https://cryptomator.org/downloads/#winDownload
DefaultDirName={pf}\Cryptomator
DefaultGroupName=Cryptomator
DisableStartupPrompt=Yes
DisableDirPage=No
DisableProgramGroupPage=Yes
DisableReadyPage=Yes
DisableFinishedPage=No
DisableWelcomePage=Yes
;Optional License
LicenseFile=license.rtf
;Win7 SP1 or above
MinVersion=6.1.7601
OutputBaseFilename=Cryptomator-{#AppVersion}-x64
;TODO
Compression=lzma2/ultra
SolidCompression=yes
PrivilegesRequired=admin
SetupIconFile=setup.ico
UninstallDisplayIcon={app}\Cryptomator.ico
UninstallDisplayName=Cryptomator
VersionInfoVersion={#FileInfoVersion}
WizardImageFile=setup-welcome.bmp
WizardImageStretch=Yes
WizardSmallImageFile=setup-banner-icon.bmp
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"

[Components]
Name: "main"; Description: "Cryptomator"; Types: full compact custom; Flags: fixed
Name: "dokan"; Description: "Dokan File System Driver"; Check: not FileExists(ExpandConstant('{sys}\drivers\dokan1.sys')); Types: full

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Services\WebClient\Parameters"; ValueType: dword; ValueName: "FileSizeLimitInBytes"; ValueData: "$ffffffff"

[InstallDelete]
Type: filesandordirs; Name: "{app}\app"
Type: filesandordirs; Name: "{app}\runtime"

[UninstallDelete]
Type: filesandordirs; Name: "{userappdata}\Cryptomator"

[Files]
Source: "Cryptomator\Cryptomator.exe"; DestDir: "{app}"; Flags: ignoreversion sign
Source: "Cryptomator\*.dll"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs signonce
Source: "Cryptomator\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Dokan_x64.msi"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall nocompression; Components: dokan

[Icons]
Name: "{group}\Cryptomator"; Filename: "{app}\Cryptomator.exe"; IconFilename: "{app}\Cryptomator.ico"

[Run]
Filename: "msiexec.exe"; Parameters: "/i ""{tmp}\Dokan_x64.msi"""; StatusMsg: "Installing Dokan Driver..."; Flags: waituntilterminated; Components: dokan; Check: DokanDependencyCheck
Filename: "net"; Parameters: "stop webclient"; StatusMsg: "Stopping WebClient..."; Flags: waituntilterminated runhidden
Filename: "net"; Parameters: "start webclient"; StatusMsg: "Restarting WebClient..."; Flags: waituntilterminated runhidden
Filename: "{app}\Cryptomator.exe"; Description: "{cm:LaunchProgram,Cryptomator}"; Flags: nowait postinstall skipifsilent

[Code]
const
  RegNetworkProviderOrderSubkey = 'SYSTEM\CurrentControlSet\Control\NetworkProvider\Order';
  RegProviderOrderValueName = 'ProviderOrder';
  RegWebClientValue = 'webclient';
  RegVcRedistKey = 'SOFTWARE\Wow6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x64';

function DokanDependencyCheck(): Boolean;
var
  major, minor, build: Cardinal;
begin
  Result := False;
  if RegKeyExists(HKEY_LOCAL_MACHINE, RegVcRedistKey) then
  begin
    if RegQueryDWordValue(HKEY_LOCAL_MACHINE, RegVcRedistKey, 'Major', major) and RegQueryDWordValue(HKEY_LOCAL_MACHINE, RegVcRedistKey, 'Minor', minor) and RegQueryDWordValue(HKEY_LOCAL_MACHINE, RegVcRedistKey, 'Bld', build) then
	begin
      // Version info was found. Return true if later or equal to our 14.11.25325
      Result := (major >= 14) and (minor >= 11) and (build >= 25325)
    end;   
  end;
  if not Result then
  begin
    MsgBox('The Dokan driver requires you to install Microsoft Visual C++ Redistributable 2017. Cryptomator installation will continue, but you will not be able to use Dokan-based drives.', mbInformation, MB_OK);
  end;
end;

function StrSplit(Text: String; Separator: String): TArrayOfString;
var
  i, p: Integer;
  Dest: TArrayOfString;
begin
  i := 0;
  repeat
    SetArrayLength(Dest, i + 1);
    p := Pos(Separator, Text);
    if p > 0 then
    begin
      Dest[i] := Copy(Text, 1, p - 1);
      Text := Copy(Text, p + Length(Separator), Length(Text));
      i := i + 1;
    end
    else
    begin
      Dest[i] := Text;
      Text := '';
    end;
  until Length(Text) = 0;
  Result := Dest
end;

procedure PatchProviderOrderRegValue();
var
  i: Integer;
  OldValue: String;
  Values: TArrayOfString;
  WebClientValueFound: Boolean;
begin
  OldValue := ExpandConstant('{reg:HKLM\' + RegNetworkProviderOrderSubkey + ',' + RegProviderOrderValueName + '|}');
  Values := StrSplit(OldValue, ',');
  if (GetArrayLength(Values) = 1) and (CompareStr(Values[0], '') = 0) then
  begin
    RegWriteStringValue(HKEY_LOCAL_MACHINE, RegNetworkProviderOrderSubkey, RegProviderOrderValueName, RegWebClientValue);
  end
  else
  begin
    WebClientValueFound := False;
    for i := 0 to GetArrayLength(Values) - 1 do
    begin
      if CompareStr(RegWebClientValue, Values[i]) = 0 then
      begin
        WebClientValueFound := True;
      end;
    end;
    if not WebClientValueFound then
    begin
      RegWriteStringValue(HKEY_LOCAL_MACHINE, RegNetworkProviderOrderSubkey, RegProviderOrderValueName, RegWebClientValue + ',' + OldValue);
    end;
  end;
end;

procedure PatchHostsFile();
var
  contents: TStringList;
  filename, statement: String;
begin
  filename := ExpandConstant('{sys}\drivers\etc\hosts');
  Log('Reading ' + filename);
  contents := TStringList.Create();
  if(FileExists(filename)) then begin
    contents.LoadFromFile(filename);
  end;

  statement := '127.0.0.1 cryptomator-vault';
  if(contents.IndexOf(statement) < 0) then begin
    Log('Adding' + statement);
    contents.Append(statement);
    try
      contents.SaveToFile(filename);
    except
      MsgBox('Unable to write to ' + filename + '.  To improve compatibility with Windows, we''d advise you to add this line manually:' + #13#10#13#10 + statement + #13#10#13#10 + 'Installation will continue after pressing OK.', mbInformation, MB_OK);
    end;
  end;
end;

function InitializeSetup(): Boolean;
begin
// Possible future improvements:
//   if version less or same => just launch app
//   if upgrade => check if same app is running and wait for it to exit
//   Add pack200/unpack200 support?
  Result := true;
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  PatchHostsFile();
  PatchProviderOrderRegValue();
  Result := '';
end;
