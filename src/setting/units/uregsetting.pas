unit uRegSetting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, uSetting;

type

  { TRegSetting }

  TRegSetting = class(TInterfacedObject, ISetting)
  private
    FRegistry: TRegistry;
  public
    function GetBackupTimeout: dword;
    function GetBaudRate: dword;
    function GetByteSize: byte;
    function GetDeveloperImage: string;
    function GetDeveloperLibrary: string;
    function GetIp: string;
    function GetKeepAliveAddress: byte;
    function GetKeepAliveTimeout: dword;
    function GetLastLink: string;
    function GetParity: byte;
    function GetPort: word;
    function GetRepeats: byte;
    function GetResponseTimeout: dword;
    function GetServerPort: word;
    function GetStopBits: byte;
    function GetThreeAndHalf: byte;
    function GetTimeout: dword;
    function GetUpdateEveryStart: boolean;
    function GetUpdateEveryWeek: boolean;
    function GetUserData: string;
    function GetUserImage: string;
    function GetUserLibrary: string;
    procedure SetBackupTimeout(const aBackupTimeout: dword);
    procedure SetBaudRate(const aBaudRate: dword);
    procedure SetByteSize(const aByteSize: byte);
    procedure SetDeveloperImage(const aDeveloperImage: string);
    procedure SetDeveloperLibrary(const aDeveloperLibrary: string);
    procedure SetIp(const aIp: string);
    procedure SetKeepAliveAddress(const aKeepAliveAddress: byte);
    procedure SetKeepAliveTimeout(const aKeepAliveTimeout: dword);
    procedure SetLastLink(AValue: string);
    procedure SetParity(const aParity: byte);
    procedure SetPort(const aPort: word);
    procedure SetRepeats(const aRepeats: byte);
    procedure SetResponseTimeout(const aResponseTimeout: dword);
    procedure SetServerPort(const aPort: word);
    procedure SetStopBits(const aStopBits: byte);
    procedure SetThreeAndHalf(const aThreeAndHalf: byte);
    procedure SetTimeout(const aTimeout: dword);
    procedure SetUpdateEveryStart(AValue: boolean);
    procedure SetUpdateEveryWeek(AValue: boolean);
    procedure SetUserImage(const aUserImage: string);
    procedure SetUserLibrary(const aUserLibrary: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRegSetting }

constructor TRegSetting.Create;
begin
  inherited Create;
  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('SOFTWARE\Jet Logic', True);
end;

destructor TRegSetting.Destroy;
begin
  FRegistry.CloseKey;
  FRegistry.Free;
  inherited Destroy;
end;

function TRegSetting.GetBackupTimeout: dword;
begin

end;

function TRegSetting.GetBaudRate: dword;
begin
  if not FRegistry.ValueExists('Baud Rate') then
    Exit(19200);
  Result := FRegistry.ReadInteger('Baud Rate');
end;

function TRegSetting.GetByteSize: byte;
begin
  if not FRegistry.ValueExists('Byte Size') then
    Exit(8);
  Result := FRegistry.ReadInteger('Byte Size');
end;

function TRegSetting.GetDeveloperImage: string;
begin

end;

function TRegSetting.GetDeveloperLibrary: string;
begin

end;

function TRegSetting.GetIp: string;
begin

end;

function TRegSetting.GetKeepAliveAddress: byte;
begin
  if not FRegistry.ValueExists('KeepAlive Address') then
    Exit(246);
  Result := FRegistry.ReadInteger('KeepAlive Address');
end;

function TRegSetting.GetKeepAliveTimeout: dword;
begin
  if not FRegistry.ValueExists('KeepAlive Timeout') then
    Exit(5000);
  Result := FRegistry.ReadInteger('KeepAlive Timeout');
end;

function TRegSetting.GetLastLink: string;
begin

end;

function TRegSetting.GetParity: byte;
begin

end;

function TRegSetting.GetPort: word;
begin

end;

function TRegSetting.GetRepeats: byte;
begin

end;

function TRegSetting.GetResponseTimeout: dword;
begin

end;

function TRegSetting.GetServerPort: word;
begin

end;

function TRegSetting.GetStopBits: byte;
begin

end;

function TRegSetting.GetThreeAndHalf: byte;
begin

end;

function TRegSetting.GetTimeout: dword;
begin

end;

function TRegSetting.GetUpdateEveryStart: boolean;
begin

end;

function TRegSetting.GetUpdateEveryWeek: boolean;
begin

end;

function TRegSetting.GetUserData: string;
begin

end;

function TRegSetting.GetUserImage: string;
begin

end;

function TRegSetting.GetUserLibrary: string;
begin

end;

procedure TRegSetting.SetBackupTimeout(const aBackupTimeout: dword);
begin

end;

procedure TRegSetting.SetBaudRate(const aBaudRate: dword);
begin
  FRegistry.WriteInteger('BaudRate', aBaudRate);
end;

procedure TRegSetting.SetByteSize(const aByteSize: byte);
begin

end;

procedure TRegSetting.SetDeveloperImage(const aDeveloperImage: string);
begin

end;

procedure TRegSetting.SetDeveloperLibrary(const aDeveloperLibrary: string);
begin

end;

procedure TRegSetting.SetIp(const aIp: string);
begin

end;

procedure TRegSetting.SetKeepAliveAddress(const aKeepAliveAddress: byte);
begin

end;

procedure TRegSetting.SetKeepAliveTimeout(const aKeepAliveTimeout: dword);
begin

end;

procedure TRegSetting.SetLastLink(AValue: string);
begin

end;

procedure TRegSetting.SetParity(const aParity: byte);
begin

end;

procedure TRegSetting.SetPort(const aPort: word);
begin

end;

procedure TRegSetting.SetRepeats(const aRepeats: byte);
begin

end;

procedure TRegSetting.SetResponseTimeout(const aResponseTimeout: dword);
begin

end;

procedure TRegSetting.SetServerPort(const aPort: word);
begin

end;

procedure TRegSetting.SetStopBits(const aStopBits: byte);
begin

end;

procedure TRegSetting.SetThreeAndHalf(const aThreeAndHalf: byte);
begin

end;

procedure TRegSetting.SetTimeout(const aTimeout: dword);
begin

end;

procedure TRegSetting.SetUpdateEveryStart(AValue: boolean);
begin

end;

procedure TRegSetting.SetUpdateEveryWeek(AValue: boolean);
begin

end;

procedure TRegSetting.SetUserImage(const aUserImage: string);
begin

end;

procedure TRegSetting.SetUserLibrary(const aUserLibrary: string);
begin

end;

end.

