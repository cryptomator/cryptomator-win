;This file will be executed next to the application bundle image
;I.e. current directory will contain folder Cryptomator with application files
[Setup]
#define AppVersion GetEnv("CRYPTOMATOR_VERSION")
#define FileInfoVersion GetFileVersion("Cryptomator/Cryptomator.exe")
#define BundledDokanVersion GetEnv("DOKAN_VERSION")
;Version of the Programmatic Identifier the app uses. Windows extra, not the same as application version. 
#define ProgIDVersion 1 

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
DefaultDirName={commonpf}\Cryptomator
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
AlwaysShowComponentsList=Yes

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"

[Types]
Name: "full"; Description:"Full Installation"
Name: "custom"; Description:"Custom Installation"; Flags: iscustom

[Components]
Name: "main"; Description: "Cryptomator"; Types: full custom; Flags: fixed
Name: "dokan"; Description: "Dokan File System Driver"; Types: full; Flags: disablenouninstallwarning
Name: "webdav"; Description: "WebDAV system configuration"; Types: full; ExtraDiskSpaceRequired: 50; Flags: disablenouninstallwarning

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Services\WebClient\Parameters"; ValueType: dword; ValueName: "FileSizeLimitInBytes"; ValueData: "$ffffffff"; Components: webdav

; Remove legacy AutoStart entry; see: https://github.com/cryptomator/cryptomator-win/issues/33
Root: HKCU; Subkey: "SOFTWARE\Microsoft\Windows\CurrentVersion\Run\"; ValueType: none; ValueName: "Cryptomator"; Flags: dontcreatekey deletevalue

; Remove legacy ProgIDs from before 1.5.12
Root: HKCR; Subkey: "CryptomatorMasterkeyFile"; Flags: dontcreatekey deletekey
Root: HKCR; Subkey: "CryptomatorEncryptedData"; Flags: dontcreatekey deletekey

; .cryptomator filetype: Add extension, MIME type, perceived type, description, icon and command
Root: HKA; Subkey: "Software\Classes\.cryptomator"; ValueType: string; ValueName: ""; ValueData: "Cryptomator.MasterkeyFile.{#ProgIDVersion}"
Root: HKA; Subkey: "Software\Classes\.cryptomator"; ValueType: string; ValueName: "Content Type"; ValueData: "application/x-vnd.cryptomator.vault-metadata"
Root: HKA; Subkey: "Software\Classes\.cryptomator"; ValueType: string; ValueName: "PerceivedType"; ValueData: "text"

Root: HKA; Subkey: "Software\Classes\Cryptomator.MasterkeyFile.{#ProgIDVersion}"; ValueType: string; ValueName: ""; ValueData: "Cryptomator Masterkey File"; Flags: deletekey uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Cryptomator.MasterkeyFile.{#ProgIDVersion}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Cryptomator.exe,0"; Flags: deletekey uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Cryptomator.MasterkeyFile.{#ProgIDVersion}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Cryptomator.exe"" ""%1"""; Flags: deletekey uninsdeletekey

; .c9r & .c9s filetypes: Add extension, MIME type, perceived type, description and icon for each
Root: HKA; Subkey: "Software\Classes\.c9r"; ValueType: string; ValueName: ""; ValueData: "Cryptomator.EncryptedData.{#ProgIDVersion}"
Root: HKA; Subkey: "Software\Classes\.c9r"; ValueType: string; ValueName: "Content Type"; ValueData: "application/vnd.cryptomator.encrypted"
Root: HKA; Subkey: "Software\Classes\.c9r"; ValueType: string; ValueName: "PerceivedType"; ValueData: "system"

Root: HKA; Subkey: "Software\Classes\.c9s"; ValueType: string; ValueName: ""; ValueData: "Cryptomator.EncryptedData.{#ProgIDVersion}"
Root: HKA; Subkey: "Software\Classes\.c9s"; ValueType: string; ValueName: "Content Type"; ValueData: "application/vnd.cryptomator.encrypted"
Root: HKA; Subkey: "Software\Classes\.c9s"; ValueType: string; ValueName: "PerceivedType"; ValueData: "system"

Root: HKA; Subkey: "Software\Classes\Cryptomator.EncryptedData.{#ProgIDVersion}"; ValueType: string; ValueName: ""; ValueData: "Cryptomator Encrypted Data"; Flags: deletekey uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Cryptomator.EncryptedData.{#ProgIDVersion}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Cryptomator.exe,0"; Flags: deletekey uninsdeletekey

[InstallDelete]
Type: filesandordirs; Name: "{app}\app"
Type: filesandordirs; Name: "{app}\runtime"

[Files]
Source: "Cryptomator\Cryptomator.exe"; DestDir: "{app}"; Flags: ignoreversion sign;
Source: "Cryptomator\*.dll"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs signonce
Source: "Cryptomator\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Dokan_x64.msi"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall nocompression; Components: dokan

[Icons]
Name: "{group}\Cryptomator"; Filename: "{app}\Cryptomator.exe"; IconFilename: "{app}\Cryptomator.ico"

[Run]
Filename: "msiexec.exe"; Parameters: {code:SelectDokanInstallerOptions}; StatusMsg: "Installing Dokan Driver..."; Flags: waituntilterminated; Components: dokan; Check: not IsInstalledDokanVersionSufficient;
Filename: "net"; Parameters: "stop webclient"; StatusMsg: "Stopping WebClient..."; Flags: waituntilterminated runhidden; Components: webdav
Filename: "net"; Parameters: "start webclient"; StatusMsg: "Restarting WebClient..."; BeforeInstall: PrepareForWebDAV; Flags: waituntilterminated runhidden; Components: webdav
Filename: "{app}\Cryptomator.exe"; Description: "{cm:LaunchProgram,Cryptomator}"; Flags: nowait postinstall skipifsilent

[Code]
const
  RegNetworkProviderOrderSubkey = 'SYSTEM\CurrentControlSet\Control\NetworkProvider\Order';
  RegProviderOrderValueName = 'ProviderOrder';
  RegWebClientValue = 'webclient';
  BundledDokanVersion = '{#BundledDokanVersion}';
  
var
  OriginalTypesChangeListener: TNotifyEvent;


// Misc section

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



// System section

procedure PatchProviderOrderRegValue();
var
  i: Integer;
  OldValue: String;
  Values: TArrayOfString;
  WebClientValueFound: Boolean;
begin
  OldValue := ExpandConstant('{reg:HKLM\' + RegNetworkProviderOrderSubkey + ',' + RegProviderOrderValueName + '|}');
  Values := StrSplit(OldValue, ',');
  if (GetArrayLength(Values) = 1) and (CompareStr(Values[0], '') = 0) then begin
    RegWriteStringValue(HKEY_LOCAL_MACHINE, RegNetworkProviderOrderSubkey, RegProviderOrderValueName, RegWebClientValue);
  end
  else
  begin
    WebClientValueFound := False;
    for i := 0 to GetArrayLength(Values) - 1 do begin
      if CompareStr(RegWebClientValue, Values[i]) = 0 then begin
        WebClientValueFound := True;
      end;
    end;
    if not WebClientValueFound then begin
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


// Dokan section

{ Calling DokanVersion() function from the dll. If it does not exists, an exception is thrown. The version number returned contains three digits.}
function DokanVersion (): Integer;
external 'DokanVersion@dokan1.dll stdcall setuponly delayload';


{ Returns the Versionnumber from the installed Dokan library or returns -1, if this is not possible.}
function ReadDokanVersion(): Int64;
begin
  try
    Result := DokanVersion();
  except
    begin
    Result := -1;
    end;
  end;
end;


{ Compares an (possibly) installed Dokan version with the bundled one and returns true if the installed on is equal or newer.}
function IsInstalledDokanVersionSufficient(): Boolean;
var
  InstalledVersion: Int64;
  BundledVersion: Int64;
  TmpString: String;
begin
	Result := False;
  InstalledVersion := ReadDokanVersion();
  if InstalledVersion >= 0 then begin
    TmpString := Trim(BundledDokanVersion);
    StringChangeEx(TmpString, '.', '',False);
    Delete(TmpString,4,1000); //we rely on that major, minor and patch never become two digit numbers
    BundledVersion := StrToInt64Def(TmpString, 9223372036854775806); //if this fails for whatever reason we just say we have the better version
    if InstalledVersion >= BundledVersion then begin
      Result := True;
    end;
  end;
end;


// Wizard section

function InitializeSetup(): Boolean;
var
  Version: TWindowsVersion;
  S: String;
begin
  GetWindowsVersionEx(Version);
  
  // Show warning for legacy Windows versions
  if Version.Major < 10 then begin
    SuppressibleMsgBox('This version of Windows is not officially supported. Proceed at your own risk.', mbInformation, MB_OK, IDOK);
  end;
  Result := True;
end;


procedure UpdateComponentsDependingOnDokany();
begin
  if ((not IsInstalledDokanVersionSufficient()) and (ReadDokanVersion <> -1)) then begin
    WizardForm.ComponentsList.ItemCaption[1] := 'Dokan File System Driver (outdated)';
  end else begin
    WizardForm.ComponentsList.ItemCaption[1] := 'Dokan File System Driver';
  end;
end;


procedure TypesChanged(Sender: TObject);
begin
  { First let Inno Setup update the components selection }
  OriginalTypesChangeListener(Sender);
  { And then check for changes }
  UpdateComponentsDependingOnDokany();
end;


procedure InitializeWizard();
begin
  OriginalTypesChangeListener := WizardForm.TypesCombo.OnChange;
  WizardForm.TypesCombo.OnChange := @TypesChanged;
  UpdateComponentsDependingOnDokany();
end;


function WizardVerySilent: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to ParamCount do
    if CompareText(ParamStr(i), '/verysilent') = 0 then
    begin
      Result := True;
      Break;
    end;
end;


function SelectDokanInstallerOptions(Param: String): String;
var
  DokanInstallerPath: String;
begin
  DokanInstallerPath := ExpandConstant('{tmp}\Dokan_x64.msi');
  Result := '/i "'+dokanInstallerPath+'"';
  if WizardSilent then
  begin
    if WizardVerySilent then begin
      Result := Result + ' /qn'; //show absolutely nuthin'
    end else begin
      Result := Result + ' /passive';
    end;
  end;
end;


function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  DokanComponentSelected: Boolean;
  DokanPresentOnSystem: Boolean;
  ContinueAnswer : Integer;
begin
    Result := '';
    DokanPresentOnSystem := (ReadDokanVersion <> -1);
    DokanComponentSelected := WizardIsComponentSelected('dokan');
    if DokanPresentOnSystem then begin 
      if DokanComponentSelected then begin 
        if not IsInstalledDokanVersionSufficient() then begin
          Result := 'The installer detected a selected, but outdated component: Dokan File System Driver.'#13#10'To update it, uninstall/remove "Dokan Library" (e.g. via "Apps and features") from your system and perform a reboot. Afterwards you can continue with the installation.'#13#10#13#10'This requirement can be disabled by deselecting the "Dokan File System Driver" component in the components selection screen.';
        end;
      end else begin
        //inform user about risk
        ContinueAnswer := MsgBox('We detected Dokany is already installed on your system, but it is not a selected component. Cryptomator will be able to use it, but it might be an outdated version missing important bug fixes and improvements. Do you still want to continue? ', mbConfirmation, MB_YESNO or MB_DEFBUTTON2);
        if ContinueAnswer = IDNO then begin
          Result := 'Installation stopped by user.'; 
        end;
      end;
    end;
end;

