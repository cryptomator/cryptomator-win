;This file will be executed next to the application bundle image
;I.e. current directory will contain folder Cryptomator with application files
[Setup]
SignTool=default sign /sha1 BAF9137F110811A5251BEB9BD6A929C2CC73E19C /tr http://timestamp.comodoca.com /v /fd sha256 /d $qCryptomator$q $f
AppId={{Cryptomator}}
AppName=Cryptomator
AppVersion=${project.version}
AppVerName=Cryptomator ${project.version}
AppPublisher=cryptomator.org
AppCopyright=cryptomator.org
AppPublisherURL=https://cryptomator.org/
AppSupportURL=https://cryptomator.org/help/
AppUpdatesURL=https://cryptomator.org/downloads/#winDownload
DefaultDirName={pf}\Cryptomator
DisableStartupPrompt=Yes
DisableDirPage=No
DisableProgramGroupPage=Yes
DisableReadyPage=Yes
DisableFinishedPage=No
DisableWelcomePage=Yes
DefaultGroupName=cryptomator.org
;Optional License
LicenseFile=license.rtf
;Win7 SP1 or above
MinVersion=6.1.7601
OutputBaseFilename=Cryptomator-${project.version}-x64
;TODO
Compression=lzma2/ultra
SolidCompression=yes
PrivilegesRequired=admin
SetupIconFile=setup.ico
UninstallDisplayIcon={app}\Cryptomator.ico
UninstallDisplayName=Cryptomator
WizardImageFile=setup-welcome.bmp
WizardImageStretch=Yes
WizardSmallImageFile=setup-banner-icon.bmp
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Services\WebClient\Parameters"; ValueType: dword; ValueName: "FileSizeLimitInBytes"; ValueData: "$ffffffff"

[InstallDelete]
Type: filesandordirs; Name: "{app}\app"
Type: filesandordirs; Name: "{app}\runtime"

[UninstallDelete]
Type: filesandordirs; Name: "{userappdata}\Cryptomator"

[Files]
Source: "Cryptomator\Cryptomator.exe"; DestDir: "{app}"; Flags: ignoreversion sign
Source: "Cryptomator\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\Cryptomator"; Filename: "{app}\Cryptomator.exe"; IconFilename: "{app}\Cryptomator.ico"

[Run]
Filename: "{app}\Cryptomator.exe"; Description: "{cm:LaunchProgram,Cryptomator}"; Flags: nowait postinstall skipifsilent
Filename: "net"; Parameters: "stop webclient"; StatusMsg: "Stopping WebClient..."; Flags: waituntilterminated runhidden
Filename: "net"; Parameters: "start webclient"; StatusMsg: "Restarting WebClient..."; Flags: waituntilterminated runhidden

[Code]
const
  RegNetworkProviderOrderSubkey = 'SYSTEM\CurrentControlSet\Control\NetworkProvider\Order';
  RegProviderOrderValueName = 'ProviderOrder';
  RegWebClientValue = 'webclient';

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
