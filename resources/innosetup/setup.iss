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
  RegDokanDisplayName = 'Dokan Library';
  BundledDokanVersion = '{#BundledDokanVersion}';
  
var
  OriginalTypesChangeListener: TNotifyEvent;
  LastKnownDokanRegistrySubKeyName: String;

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


{ If Dokany is installed and its installation has a registry entry, this function returns the version string stored in the registry. If there is no registry entry for Dokany, it returns the empty string.}
function ReadRegisteredDokanVersion(): String;
var
  I: Integer;
  Names: TArrayOfString;
  RegKey: String;
  RegValue: String;
  TruncatedName: String;
  InstalledDokanVersion: String;
begin
  Result := '';
  RegValue := '';

  { Check if we already found the registry location}
  RegKey := LastKnownDokanRegistrySubKeyName
  if (RegQueryStringValue(HKEY_LOCAL_MACHINE, RegKey, 'DisplayName', RegValue)) then
  begin
    TruncatedName := Copy(RegValue, 1, Length(RegDokanDisplayName))
    if(CompareStr(TruncatedName, RegDokanDisplayName) = 0) then 
    begin
      if (RegQueryStringValue(HKEY_LOCAL_MACHINE, RegKey, 'FullVersion', InstalledDokanVersion)) then
      begin
        Result := InstalledDokanVersion;
      end;
    end;
  end
  else if RegGetSubkeyNames(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall', Names) then
  begin {Otherwise loop over all subkeys of the uninstall key}
    for I := 0 to GetArrayLength(Names) - 1 do
    begin
      RegKey := 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + Names[I];
      if (RegQueryStringValue(HKEY_LOCAL_MACHINE, RegKey, 'DisplayName', RegValue)) then
      begin
        TruncatedName := Copy(RegValue, 1, Length(RegDokanDisplayName))
        if(CompareStr(TruncatedName, RegDokanDisplayName) = 0) then 
        begin
          //We found it!
          LastKnownDokanRegistrySubKeyName := RegKey;
          if (RegQueryStringValue(HKEY_LOCAL_MACHINE, RegKey, 'FullVersion', InstalledDokanVersion)) then
          begin
            Result := InstalledDokanVersion;
          end;
          break;
        end;
      end;
    end;
  end;
end;


{ Compares an (possibly) installed Dokan version with the bundled one and returns true if the installed on is equal or newer.}
function IsInstalledDokanVersionSufficient(): Boolean;
var
  InstalledVersionString: String;
  InstalledVersion: TArrayOfString;
  BundledVersion: TArrayOfString;
  IMajorVersion, IMinorVersion, IPatchVersion: Integer;
  BMajorVersion, BMinorVersion, BPatchVersion: Integer;
  FoundSufficientVersion: Boolean;
  TestVar : Integer;
begin
	FoundSufficientVersion := False;
  InstalledVersionString := ReadRegisteredDokanVersion();
  if (Length(InstalledVersionString) > 0) then
  begin
    BundledVersion := StrSplit(Trim(BundledDokanVersion), '.');
	  InstalledVersion := StrSplit(Trim(InstalledVersionString), '.');
	  if GetArrayLength(InstalledVersion) >= 3 then
	  begin
	    IMajorVersion := StrToIntDef(InstalledVersion[0], 0);
		  IMinorVersion := StrToIntDef(InstalledVersion[1], 0);
		  IPatchVersion := StrToIntDef(InstalledVersion[2], 0);
	    BMajorVersion := StrToIntDef(BundledVersion[0], 0);
		  BMinorVersion := StrToIntDef(BundledVersion[1], 0);
		  BPatchVersion := StrToIntDef(BundledVersion[2], 0);
      if (IMajorVersion > BMajorVersion) then
      begin
        FoundSufficientVersion := true;
      end
      else if (IMajorVersion = BMajorVersion) and (IMinorVersion > BMinorVersion) then
      begin
        FoundSufficientVersion := true;
      end
      else if (IMajorVersion = BMajorVersion) and (IMinorVersion = BMinorVersion) and (IPatchVersion >= BPatchVersion) then
      begin
        FoundSufficientVersion := true;
      end;
    end;
  end;
  Result := FoundSufficientVersion;
end;


function NextButtonClick(CurPageID: Integer): Boolean;
var
  DokanDriverLocation: String;
begin
  Result := True;
  DokanDriverLocation := ExpandConstant('{sys}\drivers\dokany1.sys');
  if (CurPageID = wpSelectComponents) and WizardIsComponentSelected('dokan') and FileExists(DokanDriverLocation) and (not IsInstalledDokanVersionSufficient()) then
  begin
    {block installation if dokan is installed and old version detected}
	MsgBox('We detected an outdated Dokany version on your system. Please uninstall it first via the "Apps & Feature" settings and perform a reboot. Afterwards continue with the installation.', mbInformation, MB_OK);
	Result := False;
  end
  else
  begin
    Result := True;
  end;
end;


procedure UpdateComponentsDependingOnDokany();
begin
  if IsInstalledDokanVersionSufficient() then
  begin
    WizardForm.ComponentsList.ItemCaption[1] := 'Dokan File System Driver (Already Installed)';
    WizardForm.ComponentsList.Checked[1] := false;
    Wizardform.ComponentsList.ItemEnabled[1] := false;
  end
  else
  begin
    WizardForm.ComponentsList.ItemCaption[1] := 'Dokan File System Driver';
    WizardForm.ComponentsList.Checked[1] := true;
    Wizardform.ComponentsList.ItemEnabled[1] := true;
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
  
  {We initialize the last known dokan registry entry to "none"}
  LastKnownDokanRegistrySubKeyName := '';
  Result := True;
end;
