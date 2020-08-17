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

[Icons]
Name: "{group}\Cryptomator"; Filename: "{app}\Cryptomator.exe"; IconFilename: "{app}\Cryptomator.ico"

[Run]
Filename: "msiexec.exe"; Parameters: "/i ""{tmp}\Dokan_x64.msi"""; StatusMsg: "Installing Dokan Driver..."; Flags: waituntilterminated; Components: dokan; 
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
  if InstalledVersion >= 0 then
  begin
    TmpString := Trim(BundledDokanVersion);
    StringChangeEx(TmpString, '.', '',False);
    Delete(TmpString,4,1000); //we rely on that major, minor and patch never become two digit numbers
    BundledVersion := StrToInt64Def(TmpString, 9223372036854775806); //if this fails for whatever reason we just say we have the better version
    if InstalledVersion >= BundledVersion then
    begin
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
  if Version.Major < 10 then
  begin
    SuppressibleMsgBox('This version of Windows is not officially supported. Proceed at your own risk.', mbInformation, MB_OK, IDOK);
  end;
  
  Result := True;
end;


procedure UpdateComponentsDependingOnDokany();
begin
  if IsInstalledDokanVersionSufficient() then
  begin
    WizardForm.ComponentsList.ItemCaption[1] := 'Dokan File System Driver (up-to-date)';
    WizardForm.ComponentsList.Checked[1] := false;
    Wizardform.ComponentsList.ItemEnabled[1] := false;
  end
  else
  begin
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


function NextButtonClick(CurPageID: Integer): Boolean;
var
  DokanComponentSelected: Boolean;
  DokanPresentOnSystem: Boolean;
  OutdatedAnswer : Integer;
begin
  Result := True;
  if ( CurPageID = wpSelectComponents) then
  begin
    DokanPresentOnSystem := (ReadDokanVersion <> -1);
    DokanComponentSelected := WizardIsComponentSelected('dokan');
    if DokanPresentOnSystem then begin 
      if DokanComponentSelected then begin 
        // we already now that it must be outdated (see UpdateComponentsDependingOnDokany) -> block installation
        MsgBox('We detected an outdated Dokany version on your system. Please uninstall it first via the "Apps & Feature" settings and perform a reboot. Afterwards continue with the installation.', mbInformation, MB_OK);
        Result := False
      end else begin
        //TODO: inform user about risk
        OutdatedAnswer := MsgBox('We detected an outdated Dokany version on your system, but it was not selected to update it. Cryptomator will most probably be able to load it, but its usage might result in errors. Do you still want to continue? ', mbConfirmation, MB_YESNO or MB_DEFBUTTON2);
        if OutdatedAnswer = IDNO then begin
          Result := False; 
        end;
      end;
    end;
  end;
end;

