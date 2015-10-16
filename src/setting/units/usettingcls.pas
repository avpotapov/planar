unit uSettingCls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles, Registry, uSetting;

type

  { TSetting }

  TSetting = class(TInterfacedObject, ISetting)
  private
    fFileName: string;
    fIniFile: TIniFile;
    fCurrentPath: string;
    fDeveloperLibrary: string;
    fDeveloperImage : string;
   fUserImage : string;
   fUserLibrary  : string;


  private
    function GetKeepAliveTimeout: dword;
    function GetBackupTimeout: dword;
    function GetDeveloperImage: string;
    function GetDeveloperLibrary: string;
    function GetKeepAliveAddress: byte;
    function GetLastLink: string;
    function GetRepeats: byte;
    function GetResponseTimeout: dword;
    function GetTimeout: dword;
    function GetUpdateEveryStart: boolean;
    function GetUpdateEveryWeek: boolean;
    function GetUserData: string;
    function GetUserImage: string;
    function GetUserLibrary: string;
    procedure SetBackupTimeout(const aBackupTimeout: dword);
    procedure SetDeveloperImage(const aDeveloperImage: string);
    procedure SetDeveloperLibrary(const aDeveloperLibrary: string);
    procedure SetKeepAliveAddress(const aKeepAliveAddress: byte);
    procedure SetKeepAliveTimeout(const aKeepAliveTimeout: dword);
    procedure SetLastLink(AValue: string);
    procedure SetRepeats(const aRepeats: byte);
    procedure SetResponseTimeout(const aResponseTimeout: dword);
    procedure SetTimeout(const aTimeout: dword);
    procedure SetUpdateEveryWeek(AValue: boolean);
    procedure SetUpdateEveryStart(AValue: boolean);
    procedure SetUserImage(const aUserImage: string);
    procedure SetUserLibrary(const aUserLibrary: string);
    function GetBaudRate: dword;
    procedure SetBaudRate(const aBaudRate: dword);
    function GetParity: byte;
    procedure SetParity(const aParity: byte);
    function GetByteSize: byte;
    procedure SetByteSize(const aByteSize: byte);
    function GetStopBits: byte;
    procedure SetStopBits(const aStopBits: byte);
    function GetThreeAndHalf: byte;
    procedure SetThreeAndHalf(const aThreeAndHalf: byte);
    function GetIp: string;
    procedure SetIp(const aIp: string);
    function GetPort: word;
    procedure SetPort(const aPort: word);
    function GetServerPort: word;
    procedure SetServerPort(const aPort: word);

  public
    constructor Create(const aFileName: string = ''); reintroduce;
    procedure AfterConstruction; override;
    destructor Destroy; override;

  public
    property BaudRate: dword read GetBaudRate write SetBaudRate;
    property ByteSize: byte read GetByteSize write SetByteSize;
    property Parity: byte read GetParity write SetParity;
    property StopBits: byte read GetStopBits write SetStopBits;
    property ThreeAndHalf: byte read GetThreeAndHalf write SetThreeAndHalf;
    property Ip: string read GetIp write SetIp;
    property Port: word read GetPort write SetPort;
    property ServerPort: word read GetServerPort write SetServerPort;
    property KeepAliveAddress: byte read GetKeepAliveAddress write SetKeepAliveAddress;
    property KeepAliveTimeout: dword read GetKeepAliveTimeout write SetKeepAliveTimeout;
    property Timeout: dword read GetTimeout write SetTimeout;
    property ResponseTimeout: dword read GetResponseTimeout write SetResponseTimeout;
    property BackupTimeout: dword read GetBackupTimeout write SetBackupTimeout;
    property Repeats: byte read GetRepeats write SetRepeats;
    property DeveloperLibrary: string read GetDeveloperLibrary
      write SetDeveloperLibrary;
    property DeveloperImage: string read GetDeveloperImage write SetDeveloperImage;
    property UserLibrary: string read GetUserLibrary write SetUserLibrary;
    property UserImage: string read GetUserImage write SetUserImage;
    property UpdateEveryStart: boolean read GetUpdateEveryStart
      write SetUpdateEveryStart;
    property UpdateEveryWeek: boolean read GetUpdateEveryWeek write SetUpdateEveryWeek;
    property UserData: string read GetUserData;
    property LastLink: string read GetLastLink write SetLastLink;

  end;

implementation

{ TSetting }


function GetUserPathFromRegistry: string;
var
  Registry: TRegistry;
begin
  Result := '';
  { создаём объект TRegistry }
  Registry := TRegistry.Create;
  try
    { устанавливаем корневой ключ; напрмер hkey_local_machine или hkey_current_user }
    Registry.RootKey := HKEY_CURRENT_USER;
    { открываем ключ }
    if not Registry.OpenKey('SOFTWARE\JetLogic', True) then
      Exit;
    Result :=AnsiToUtf8(Registry.ReadString('UserData'));
    { закрываем и освобождаем ключ }
    Registry.CloseKey;
  finally
    Registry.Free;
  end;

end;

constructor TSetting.Create(const aFileName: string);
begin
  inherited Create;
  fFileName := aFileName;
  if fFileName = '' then
    fFileName := IncludeTrailingPathDelimiter(GetUserPathFromRegistry) + 'setting.ini';
  if fFileName = '' then
    fFileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'setting.ini';
end;

procedure TSetting.AfterConstruction;
begin
  inherited AfterConstruction;
  fIniFile := TIniFile.Create(Utf8ToAnsi(fFileName));
  fCurrentPath := Utf8ToAnsi(ExtractFilePath({ParamStr(0)}fFileName));
end;

destructor TSetting.Destroy;
begin
  FreeAndNil(fIniFile);
  inherited Destroy;
end;

function TSetting.GetBaudRate: dword;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'BaudRate', 19200);
end;

procedure TSetting.SetBaudRate(const aBaudRate: dword);
begin
  fIniFile.WriteInteger('MODBUS', 'BaudRate', aBaudRate);
end;

function TSetting.GetParity: byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'Parity', 2);
end;

procedure TSetting.SetParity(const aParity: byte);
begin
  fIniFile.WriteInteger('MODBUS', 'Parity', aParity);
end;

function TSetting.GetByteSize: byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'ByteSize', 8);
end;

procedure TSetting.SetByteSize(const aByteSize: byte);
begin
  fIniFile.WriteInteger('MODBUS', 'ByteSize', aByteSize);
end;

function TSetting.GetStopBits: byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'StopBits', 0);
end;

procedure TSetting.SetStopBits(const aStopBits: byte);
begin
  fIniFile.WriteInteger('MODBUS', 'StopBits', aStopBits);
end;

function TSetting.GetThreeAndHalf: byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'ThreeAndHalf', 30);
end;

procedure TSetting.SetThreeAndHalf(const aThreeAndHalf: byte);
begin
  fIniFile.WriteInteger('MODBUS', 'ThreeAndHalf', aThreeAndHalf);
end;

function TSetting.GetIp: string;
begin
  Result := fIniFile.ReadString('MODBUS', 'IP', '192.168.0.166');
end;

procedure TSetting.SetIp(const aIp: string);
begin
  fIniFile.WriteString('MODBUS', 'IP', aIp);
end;

function TSetting.GetPort: word;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'Port', 502);
end;

procedure TSetting.SetPort(const aPort: word);
begin
  fIniFile.WriteInteger('MODBUS', 'Port', aPort);
end;

function TSetting.GetServerPort: word;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'ServerPort', 502);
end;

procedure TSetting.SetServerPort(const aPort: word);
begin
  fIniFile.WriteInteger('MODBUS', 'ServerPort', aPort);
end;

function TSetting.GetKeepAliveTimeout: dword;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'KeepAliveTimeout', 5000);
end;

procedure TSetting.SetKeepAliveTimeout(const aKeepAliveTimeout: dword);
begin
  fIniFile.WriteInteger('MODBUS', 'KeepAliveTimeout', aKeepAliveTimeout);
end;

procedure TSetting.SetLastLink(AValue: string);
begin
  fIniFile.WriteString('CONFIGURATOR', 'LastLink', AValue);
end;

function TSetting.GetKeepAliveAddress: byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'KeepAliveAddress', 246);
end;

function TSetting.GetLastLink: string;
begin
  Result := fIniFile.ReadString('CONFIGURATOR', 'LastLink', 'TCP');
end;

procedure TSetting.SetKeepAliveAddress(const aKeepAliveAddress: byte);
begin
  fIniFile.WriteInteger('MODBUS', 'KeepAliveAddress', aKeepAliveAddress);
end;

function TSetting.GetTimeout: dword;
begin
  Result := fIniFile.ReadInteger('CONFIGURATOR', 'Timeout', 1000);
end;

function TSetting.GetUpdateEveryStart: boolean;
begin
  Result := fIniFile.ReadBool('CONFIGURATOR', 'UpdateEveryStart', False);
end;

function TSetting.GetUpdateEveryWeek: boolean;
begin
  Result := fIniFile.ReadBool('CONFIGURATOR', 'UpdateEveryWeek', False);
end;

function TSetting.GetUserData: string;
begin
	Result := GetUserPathFromRegistry;
end;

procedure TSetting.SetTimeout(const aTimeout: dword);
begin
  fIniFile.WriteInteger('CONFIGURATOR', 'Timeout', aTimeout);
end;

procedure TSetting.SetUpdateEveryWeek(AValue: boolean);
begin
  fIniFile.WriteBool('CONFIGURATOR', 'UpdateEveryWeek', aValue);
end;

procedure TSetting.SetUpdateEveryStart(AValue: boolean);
begin
  fIniFile.WriteBool('CONFIGURATOR', 'UpdateEveryStart', aValue);
end;

function TSetting.GetResponseTimeout: dword;
begin
  Result := fIniFile.ReadInteger('CONFIGURATOR', 'ResponseTimeout', 500);
end;

procedure TSetting.SetResponseTimeout(const aResponseTimeout: dword);
begin
  fIniFile.WriteInteger('CONFIGURATOR', 'ResponseTimeout', aResponseTimeout);
end;

function TSetting.GetBackupTimeout: dword;
begin
  Result := fIniFile.ReadInteger('CONFIGURATOR', 'BackupTimeout', 5000);
end;

procedure TSetting.SetBackupTimeout(const aBackupTimeout: dword);
begin
  fIniFile.WriteInteger('CONFIGURATOR', 'BackupTimeout', aBackupTimeout);
end;

function TSetting.GetRepeats: byte;
begin
  Result := fIniFile.ReadInteger('CONFIGURATOR', 'Repeats', 0);
end;

procedure TSetting.SetRepeats(const aRepeats: byte);
begin
  fIniFile.WriteInteger('CONFIGURATOR', 'Repeats', aRepeats);
end;

function TSetting.GetDeveloperLibrary: string;
begin
  fDeveloperLibrary := {AnsiToUtf8}(fIniFile.ReadString('LIBRARY', 'DeveloperLibrary', fCurrentPath));
  Result := fDeveloperLibrary
end;

procedure TSetting.SetDeveloperLibrary(const aDeveloperLibrary: string);
begin
  fIniFile.WriteString('LIBRARY', 'DeveloperLibrary', {Utf8ToAnsi}(aDeveloperLibrary));
end;

function TSetting.GetDeveloperImage: string;
begin
  fDeveloperImage :=  {AnsiToUtf8}(fIniFile.ReadString('LIBRARY', 'DeveloperImage', fCurrentPath));
  Result := fDeveloperImage
end;

procedure TSetting.SetDeveloperImage(const aDeveloperImage: string);
begin
  fIniFile.WriteString('LIBRARY', 'DeveloperImage', {Utf8ToAnsi}(aDeveloperImage));
end;

function TSetting.GetUserLibrary: string;
begin
  fUserLibrary := {AnsiToUtf8}(fIniFile.ReadString('LIBRARY', 'UserLibrary', fCurrentPath));
  Result := fUserLibrary;
end;

procedure TSetting.SetUserLibrary(const aUserLibrary: string);
begin
  fIniFile.WriteString('LIBRARY', 'UserLibrary', {Utf8ToAnsi}(aUserLibrary));
end;

function TSetting.GetUserImage: string;
begin
  fUserImage := {AnsiToUtf8}(fIniFile.ReadString('LIBRARY', 'UserImage', fCurrentPath));
  Result := fUserImage;
end;

procedure TSetting.SetUserImage(const aUserImage: string);
begin
  fIniFile.WriteString('LIBRARY', 'UserImage', {Utf8ToAnsi}(aUserImage));
end;

end.
