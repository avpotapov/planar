unit uSettingCls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles, uSetting;

type

  { TSetting }

  TSetting = class(TInterfacedObject, ISetting)
  private
    fFileName: string;
    fIniFile: TIniFile;
    fCurrentPath: string;
  private
    function GetKeepAliveTimeout: dword;
    function GetBackupTimeout: dword;
    function GetDeveloperImage: String;
    function GetDeveloperLibrary: String;
    function GetKeepAliveAddress: Byte;
    function GetRepeats: Byte;
    function GetResponseTimeout: dword;
    function GetTimeout: dword;
    function GetUpdateEveryStart: Boolean;
    function GetUpdateEveryWeek: Boolean;
    function GetUserImage: String;
    function GetUserLibrary: String;
    procedure SetBackupTimeout(const aBackupTimeout: dword);
    procedure SetDeveloperImage(const aDeveloperImage: String);
    procedure SetDeveloperLibrary(const aDeveloperLibrary: String);
    procedure SetKeepAliveAddress(const aKeepAliveAddress: Byte);
    procedure SetKeepAliveTimeout(const aKeepAliveTimeout: dword);
    procedure SetRepeats(const aRepeats: Byte);
    procedure SetResponseTimeout(const aResponseTimeout: dword);
    procedure SetTimeout(const aTimeout: dword);
    procedure SetUpdateEveryWeek(AValue: Boolean);
    procedure SetUpdateEveryStart(AValue: Boolean);
    procedure SetUserImage(const aUserImage: String);
    procedure SetUserLibrary(const aUserLibrary: String);
    function GetBaudRate: dword;
    procedure SetBaudRate(const aBaudRate: dword);
    function GetParity: Byte;
    procedure SetParity(const aParity: Byte);
    function GetByteSize: Byte;
    procedure SetByteSize(const aByteSize: Byte);
    function GetStopBits: Byte;
    procedure SetStopBits(const aStopBits: Byte);
    function GetThreeAndHalf: Byte;
    procedure SetThreeAndHalf(const aThreeAndHalf: Byte);
    function GetIp: String;
    procedure SetIp(const aIp: String);
    function GetPort: Word;
    procedure SetPort(const aPort: Word);
    function GetServerPort: Word;
    procedure SetServerPort(const aPort: Word);

  public
    constructor Create(const aFileName: string = ''); reintroduce;
    procedure AfterConstruction; override;
    destructor Destroy; override;

  public
    property BaudRate: dword read GetBaudRate write SetBaudRate;
    property ByteSize: Byte read GetByteSize write SetByteSize;
    property Parity: Byte read GetParity write SetParity;
    property StopBits: Byte read GetStopBits write SetStopBits;
    property ThreeAndHalf: Byte read GetThreeAndHalf write SetThreeAndHalf;
    property Ip: String read GetIp write SetIp;
    property Port: Word read GetPort write SetPort;
    property ServerPort: Word read GetServerPort write SetServerPort;
    property KeepAliveAddress: Byte read GetKeepAliveAddress write SetKeepAliveAddress;
    property KeepAliveTimeout: dword read GetKeepAliveTimeout write SetKeepAliveTimeout;
    property Timeout: dword read GetTimeout write SetTimeout;
    property ResponseTimeout: dword read GetResponseTimeout write SetResponseTimeout;
    property BackupTimeout: dword read GetBackupTimeout write SetBackupTimeout;
    property Repeats: Byte read GetRepeats write SetRepeats;
    property DeveloperLibrary: String read GetDeveloperLibrary
      write SetDeveloperLibrary;
    property DeveloperImage: String read GetDeveloperImage write SetDeveloperImage;
    property UserLibrary: String read GetUserLibrary write SetUserLibrary;
    property UserImage: String read GetUserImage write SetUserImage;
    property UpdateEveryStart: Boolean read GetUpdateEveryStart write SetUpdateEveryStart;
    property UpdateEveryWeek: Boolean read GetUpdateEveryWeek write SetUpdateEveryWeek;

  end;

implementation

{ TSetting }

constructor TSetting.Create(const aFileName: string);
begin
  inherited Create;
  if aFileName = '' then
    fFileName := 'setting.ini'
  else
    fFileName := aFileName;
end;

procedure TSetting.AfterConstruction;
begin
  inherited AfterConstruction;
  fIniFile := TIniFile.Create(fFileName);
  fCurrentPath := ExtractFilePath(ParamStr(0));
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

function TSetting.GetParity: Byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'Parity', 2);
end;

procedure TSetting.SetParity(const aParity: Byte);
begin
    fIniFile.WriteInteger('MODBUS', 'Parity', aParity);
end;

function TSetting.GetByteSize: Byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'ByteSize', 8);
end;

procedure TSetting.SetByteSize(const aByteSize: Byte);
begin
    fIniFile.WriteInteger('MODBUS', 'ByteSize', aByteSize);
end;

function TSetting.GetStopBits: Byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'StopBits', 0);
end;

procedure TSetting.SetStopBits(const aStopBits: Byte);
begin
    fIniFile.WriteInteger('MODBUS', 'StopBits', aStopBits);
end;

function TSetting.GetThreeAndHalf: Byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'ThreeAndHalf', 30);
end;

procedure TSetting.SetThreeAndHalf(const aThreeAndHalf: Byte);
begin
    fIniFile.WriteInteger('MODBUS', 'ThreeAndHalf', aThreeAndHalf);
end;

function TSetting.GetIp: String;
begin
  Result := fIniFile.ReadString('MODBUS', 'IP', '192.168.0.166');
end;

procedure TSetting.SetIp(const aIp: String);
begin
  fIniFile.WriteString('MODBUS', 'IP', aIp);
end;

function TSetting.GetPort: Word;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'Port', 502);
end;

procedure TSetting.SetPort(const aPort: Word);
begin
   fIniFile.WriteInteger('MODBUS', 'Port', aPort);
end;

function TSetting.GetServerPort: Word;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'ServerPort', 502);
end;

procedure TSetting.SetServerPort(const aPort: Word);
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

function TSetting.GetKeepAliveAddress: Byte;
begin
  Result := fIniFile.ReadInteger('MODBUS', 'KeepAliveAddress', 246);
end;

procedure TSetting.SetKeepAliveAddress(const aKeepAliveAddress: Byte);
begin
  fIniFile.WriteInteger('MODBUS', 'KeepAliveAddress', aKeepAliveAddress);
end;

function TSetting.GetTimeout: dword;
begin
  Result := fIniFile.ReadInteger('CONFIGURATOR', 'Timeout', 1000);
end;

function TSetting.GetUpdateEveryStart: Boolean;
begin
  Result := fIniFile.ReadBool('CONFIGURATOR', 'UpdateEveryStart', False);
end;

function TSetting.GetUpdateEveryWeek: Boolean;
begin
  Result := fIniFile.ReadBool('CONFIGURATOR', 'UpdateEveryWeek', False);
end;

procedure TSetting.SetTimeout(const aTimeout: dword);
begin
   fIniFile.WriteInteger('CONFIGURATOR', 'Timeout', aTimeout);
end;

procedure TSetting.SetUpdateEveryWeek(AValue: Boolean);
begin
  fIniFile.WriteBool('CONFIGURATOR', 'UpdateEveryWeek', aValue);
end;

procedure TSetting.SetUpdateEveryStart(AValue: Boolean);
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

function TSetting.GetRepeats: Byte;
begin
  Result := fIniFile.ReadInteger('CONFIGURATOR', 'Repeats', 0);
end;

procedure TSetting.SetRepeats(const aRepeats: Byte);
begin
   fIniFile.WriteInteger('CONFIGURATOR', 'Repeats', aRepeats);
end;

function TSetting.GetDeveloperLibrary: String;
begin
  Result := fIniFile.ReadString('LIBRARY', 'DeveloperLibrary',  fCurrentPath);
end;
procedure TSetting.SetDeveloperLibrary(const aDeveloperLibrary: String);
begin
  fIniFile.WriteString('LIBRARY', 'DeveloperLibrary', aDeveloperLibrary);
end;

function TSetting.GetDeveloperImage: String;
begin
  Result := fIniFile.ReadString('LIBRARY', 'DeveloperImage', fCurrentPath);
end;

procedure TSetting.SetDeveloperImage(const aDeveloperImage: String);
begin
  fIniFile.WriteString('LIBRARY', 'DeveloperImage', aDeveloperImage);
end;

function TSetting.GetUserLibrary: String;
begin
 Result := fIniFile.ReadString('LIBRARY', 'UserLibrary', fCurrentPath);
end;

procedure TSetting.SetUserLibrary(const aUserLibrary: String);
begin
  fIniFile.WriteString('LIBRARY', 'UserLibrary', aUserLibrary);
end;

function TSetting.GetUserImage: String;
begin
 Result := fIniFile.ReadString('LIBRARY', 'UserImage', fCurrentPath);
end;

procedure TSetting.SetUserImage(const aUserImage: String);
begin
  fIniFile.WriteString('LIBRARY', 'UserImage', aUserImage);
end;

end.
