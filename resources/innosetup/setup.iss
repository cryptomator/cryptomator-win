;This file will be executed next to the application bundle image
;I.e. current directory will contain folder Cryptomator with application files
[Setup]
#define AppVersion GetEnv("CRYPTOMATOR_VERSION")
#define FileInfoVersion GetFileVersion("Cryptomator/Cryptomator.exe")
#define BundledDokanVersion GetEnv("DOKAN_VERSION")

SignTool=default /tr http://timestamp.comodoca.com /fd sha256 /d $qCryptomator$q $f
AppId=Cryptomator
AppName=Cryptomator
AppVersion={#AppVersion}
AppPublisher=cryptomator.org
AppCopyright=cryptomator.org
AppPublisherURL=https://cryptomator.org/
AppSupportURL=https://cryptomator.org/support/
AppUpdatesURL=https://cryptomator.org/downloads/#winDownload
ChangesAssociations=Yes
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
WizardStyle=modern
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"

[Components]
Name: "main"; Description: "Cryptomator"; Types: full compact custom; Flags: fixed
Name: "dokan"; Description: "Dokan File System Driver"; Types: full; Flags: disablenouninstallwarning
Name: "webdav"; Description: "WebDAV system configuration"; Types: full compact; ExtraDiskSpaceRequired: 50; Flags: disablenouninstallwarning

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Services\WebClient\Parameters"; ValueType: dword; ValueName: "FileSizeLimitInBytes"; ValueData: "$ffffffff"; Components: webdav
Root: HKCR; Subkey: ".cryptomator"; ValueType: string;  ValueName: ""; ValueData: "CryptomatorMasterkeyFile"; Flags: uninsdeletekey
Root: HKCR; Subkey: "CryptomatorMasterkeyFile"; ValueType: string;  ValueName: ""; ValueData: "Cryptomator Masterkey File"; Flags: uninsdeletekey
Root: HKCR; Subkey: "CryptomatorMasterkeyFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Cryptomator.exe,0"
Root: HKCR; Subkey: "CryptomatorMasterkeyFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Cryptomator.exe"" ""%1"""

[InstallDelete]
Type: filesandordirs; Name: "{app}\app"
Type: filesandordirs; Name: "{app}\runtime"

[Files]
Source: "Cryptomator\Cryptomator.exe"; DestDir: "{app}"; Flags: ignoreversion sign;
Source: "Cryptomator\*.dll"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs signonce
Source: "Cryptomator\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Dokan_x64.msi"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall nocompression; Components: dokan
Source: "vc_redist.x64.exe"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall nocompression; Components: dokan

[Icons]
Name: "{group}\Cryptomator"; Filename: "{app}\Cryptomator.exe"; IconFilename: "{app}\Cryptomator.ico"

[Run]
Filename: "{tmp}\vc_redist.x64.exe"; Parameters: "/norestart /q /chainingpackage ADMINDEPLOYMENT"; StatusMsg: "Installing VC++ Redistributable 2019..."; Flags: waituntilterminated; Components: dokan; Check: not VCRedistInstalled
Filename: "msiexec.exe"; Parameters: "/i ""{tmp}\Dokan_x64.msi"""; StatusMsg: "Installing Dokan Driver..."; Flags: waituntilterminated; Components: dokan; 
Filename: "net"; Parameters: "stop webclient"; StatusMsg: "Stopping WebClient..."; Flags: waituntilterminated runhidden; Components: webdav
Filename: "net"; Parameters: "start webclient"; StatusMsg: "Restarting WebClient..."; BeforeInstall: PrepareForWebDAV; Flags: waituntilterminated runhidden; Components: webdav
Filename: "{app}\Cryptomator.exe"; Description: "{cm:LaunchProgram,Cryptomator}"; Flags: nowait postinstall skipifsilent

[Code]
const
  RegNetworkProviderOrderSubkey = 'SYSTEM\CurrentControlSet\Control\NetworkProvider\Order';
  RegProviderOrderValueName = 'ProviderOrder';
  RegWebClientValue = 'webclient';
  RegVcRedistKey = 'SOFTWARE\Classes\Installer\Dependencies\Microsoft.VS.VC_RuntimeMinimumVSU_amd64,v14';
  DokanNameCheck = 'Dokan Library';
  BundledDokanVersion = '{#BundledDokanVersion}';

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

function VCRedistInstalled(): Boolean;
var
  VersionString: String;
  Version: TArrayOfString;
  MajorVersion, MinorVersion, BuildVersion: Integer;
  FoundRequiredVersion: Boolean;
begin
  Result := False;
  if RegKeyExists(HKEY_LOCAL_MACHINE, RegVcRedistKey) then
  begin
    if RegQueryStringValue(HKEY_LOCAL_MACHINE, RegVcRedistKey, 'Version', VersionString)then
	begin
	  Version := StrSplit(VersionString, '.');
	  if GetArrayLength(Version) >= 3 then
	  begin
	    MajorVersion := StrToIntDef(Version[0], 0);
		MinorVersion := StrToIntDef(Version[1], 0);
		BuildVersion := StrToIntDef(Version[2], 0);
		if (MajorVersion > 14) then
		begin
		  FoundRequiredVersion := true;
		end
		else if (MajorVersion = 14) and (MinorVersion > 21) then
		begin
		  FoundRequiredVersion := true;
		end
		else if (MajorVersion = 14) and (MinorVersion = 21) and (BuildVersion >= 27702) then
		begin
		  FoundRequiredVersion := true;
		end;
	  end;
    end;   
  end;
  Result := FoundRequiredVersion;
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

procedure PrepareForWebDAV();
begin
  PatchHostsFile();
  PatchProviderOrderRegValue();
end;


function oldDokanVersionDetected(): Boolean;   //TODO: Use the dll function loading ability of inno setup
var
  I: Integer;
  Names: TArrayOfString;
  TempRegKey: String;
  TempRegValueName: String;
  TmpNameCheck: String;
  InstalledDokanVersion: String;
  FoundEntry: Boolean;
  DebugString: String;
begin
  Result := False;
  TempRegValueName := '';
  FoundEntry := False;
  if RegGetSubkeyNames(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall', Names) then
  begin
    for I := 0 to GetArrayLength(Names) - 1 do
    begin
      TempRegKey := 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+Names[I];
      if (RegQueryStringValue(HKEY_LOCAL_MACHINE, TempRegKey, 'DisplayName', TempRegValueName)) then
      begin
        TmpNameCheck := Copy(TempRegValueName,1,Length(DokanNameCheck))
        if(CompareStr(TmpNameCheck,DokanNameCheck) = 0) then 
        begin
          //We found it!
          if (RegQueryStringValue(HKEY_LOCAL_MACHINE, TempRegKey, 'FullVersion', InstalledDokanVersion)) then
          begin
             Result := (CompareText(Trim(InstalledDokanVersion), Trim(BundledDokanVersion)) < 0);
             if (Result) then begin MsgBox('We detected an older Dokany version on your system. Please uninstall it first and do a reboot. Afterwards continue with the installation.', mbInformation, MB_OK); end;
             FoundEntry := True;
          end;
        end;
      end;
      if FoundEntry = True then
      begin
          break;
      end;
    end;
  end;
end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  if CurPageID = wpSelectComponents then
  begin
    Result := not( IsComponentSelected('dokan') and oldDokanVersionDetected());
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
