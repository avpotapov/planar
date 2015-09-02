unit uframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows,
  uModbus, uBase;

type

  { TFrame }

  TFrame = class(TPriorityItem, IFrame)
  private
    fSlaveId: byte;

    fRequestCount: longint;
    fResponseCount: longint;

    fRequestPdu: TPdu;
    fResponsePdu: TPdu;

    fTimeout: dword;

    fResponseEvent: THandle;

    fString: string;
  protected
    function GetSlaveId: byte;
    procedure SetSlaveId(const aSlaveId: byte);

    function GetRequestCount: Longint;
    procedure SetRequestCount(const aCount: longint);

    function GetResponseCount: Longint;
    procedure SetResponseCount(const aCount: longint);

    function GetRequestPdu: PPdu;
    function GetResponsePdu: PPdu;

    function GetTimeout: dword;
    procedure SetTimeout(const aTimeout: dword);

    function GetResponded: boolean;
    procedure SetResponded(const aResponded: boolean);

  public
    constructor Create;
    destructor Destroy; override;

  public
    function ToString: string; reintroduce;

    property SlaveId: byte read GetSlaveId write SetSlaveId;

    property RequestCount: longint read GetRequestCount write SetRequestCount;
    property ResponseCount: longint read GetResponseCount write SetResponseCount;

    property RequestPdu: PPdu read GetRequestPdu;
    property ResponsePdu: PPdu read GetResponsePdu;

    property Timeout: dword read GetTimeout write SetTimeout;

    property Responded: boolean read GetResponded write SetResponded;

  end;

implementation

{ TFrame }

constructor TFrame.Create;
begin
  inherited Create;
  fResponseEvent := CreateEvent(nil, false, false, '');
end;

destructor TFrame.Destroy;
begin
  CloseHandle(fResponseEvent);
  inherited Destroy;
end;

function TFrame.ToString: string;
var
  RequestStr: String;
  ResponseStr: String;
  I: Integer;
begin
  RequestStr := '';
  if fRequestCount > 0 then
    for I := 0 to  fRequestCount - 1 do
      RequestStr := RequestStr + IntToHex(fRequestPdu[I], 2) + ' ';

  ResponseStr := '';
  if fResponseCount > 0 then
    for I := 0 to  fResponseCount - 1 do
      ResponseStr := ResponseStr + IntToHex(fResponsePdu[I], 2) + ' ';


  fString := '';
  fString := fString + Format('Адрес устройства: %d', [fSlaveId]) + #13#10;
  fString := fString + Format('Запрос: %s', [RequestStr]) + #13#10;
  fString := fString + Format('Ответ: %s', [ResponseStr]) + #13#10;
  Result := fString;
end;

function TFrame.GetSlaveId: byte;
begin
  Result := fSlaveId;
end;

procedure TFrame.SetSlaveId(const aSlaveId: byte);
begin
  if fSlaveId <> aSlaveId then
     fSlaveId := aSlaveId;
end;

function TFrame.GetRequestCount: Longint;
begin
  Result := fRequestCount;
end;

procedure TFrame.SetRequestCount(const aCount: longint);
begin
  if fRequestCount <> aCount then
     fRequestCount := aCount;
end;

function TFrame.GetResponseCount: Longint;
begin
  Result := fResponseCount;
end;

procedure TFrame.SetResponseCount(const aCount: longint);
begin
  if fResponseCount <> aCount then
     fResponseCount := aCount;
end;

function TFrame.GetRequestPdu: PPdu;
begin
  Result := @fRequestPdu;
end;

function TFrame.GetResponsePdu: PPdu;
begin
  Result := @fResponsePdu
end;

function TFrame.GetTimeout: dword;
begin
  Result := fTimeout;
end;

procedure TFrame.SetTimeout(const aTimeout: dword);
begin
  if fTimeout <> aTimeout then
     fTimeout := aTimeout;
end;

function TFrame.GetResponded: boolean;
begin
  Result := WaitForSingleObject(fResponseEvent, 3 * fTimeout) = WAIT_OBJECT_0;
end;

procedure TFrame.SetResponded(const aResponded: boolean);
begin
  case aResponded of
    true: SetEvent(fResponseEvent);
    false: ResetEvent(fResponseEvent);
  end;
end;

end.

