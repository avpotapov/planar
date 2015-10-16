unit uSignature;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Windows, Forms, Messages,
  uModbus, uSetting, uMap;

type
  ISignature = interface
    ['{ED315EFB-5316-46BD-AD0D-15CA6F5566C9}']
    function GetDateIdHard: String;
    function GetModificator: Byte;
    function GetSerialNumber: String;
    function GetTypeFirmWare: Word;
    function GetTypeHard: Word;
    function GetTypeIdHard: Word;
    function GetTypeProject: Word;
    function GetVersionFirmWare: String;
    function GetVersionProject: String;

    property DateIdHard: String read GetDateIdHard;
    property Modificator: Byte read GetModificator;
    property SerialNumber: String read GetSerialNumber;
    property TypeFirmWare: Word read GetTypeFirmWare;
    property TypeHard: Word read GetTypeHard;
    property TypeIdHard: Word read GetTypeIdHard;
    property TypeProject: Word read GetTypeProject;
    property VersionFirmWare: String read GetVersionFirmWare;
    property VersionProject: String read GetVersionProject;
  end;

  {$REGION Signature }

  { TSignature }

  TThreadSignature = class;
  TSignature = class(TInterfacedObject, ISignature)
  protected
    fSlaveId: Byte;
    fMap:     IMap;
    fController: IController;

    fInput:   IFrame;
    fHolding: IFrame;

    fOnUpdate: TNotifyEvent;

    fThreadSignature: TThreadSignature;

  protected
    function GetDateIdHard: String; virtual;
    function GetModificator: Byte; virtual;
    function GetSerialNumber: String; virtual;
    function GetTypeFirmWare: Word; virtual;
    function GetTypeHard: Word; virtual;
    function GetTypeIdHard: Word; virtual;
    function GetTypeProject: Word; virtual;
    function GetVersionFirmWare: String; virtual;
    function GetVersionProject: String; virtual;

    procedure CreateFrames; virtual;

  public
    constructor Create(const aController: IController; const aMap: IMap;
      const aSlaveId: Byte; const aOnUpdate: TNotifyEvent = nil);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  public
    property DateIdHard: String read GetDateIdHard;
    property Modificator: Byte read GetModificator;
    property SerialNumber: String read GetSerialNumber;
    property TypeFirmWare: Word read GetTypeFirmWare;
    property TypeHard: Word read GetTypeHard;
    property TypeIdHard: Word read GetTypeIdHard;
    property TypeProject: Word read GetTypeProject;
    property VersionFirmWare: String read GetVersionFirmWare;
    property VersionProject: String read GetVersionProject;

  end;

{$ENDREGION Signature }

{$REGION Auto}

  { TAuto }

  TAuto = class(TSignature)
  protected
    function GetDateIdHard: String; override;
    function GetModificator: Byte; override;
    function GetSerialNumber: String; override;
    function GetTypeFirmWare: Word; override;
    function GetTypeHard: Word; override;
    function GetTypeIdHard: Word; override;
    function GetTypeProject: Word; override;
    function GetVersionFirmWare: String; override;
    function GetVersionProject: String; override;
    procedure CreateFrames; override;


  end;

{$ENDREGION Auto }

{$REGION RCCU}

  { TRCCU }

  TRCCU = class(TSignature)
  protected
    function GetDateIdHard: String; override;
    function GetSerialNumber: String; override;
    function GetTypeHard: Word; override;
    function GetTypeIdHard: Word; override;
    function GetVersionFirmWare: String; override;
    procedure CreateFrames; override;

  end;

{$ENDREGION RCCU}

  { TThreadSignature }

  TThreadSignature = class(TThread)
  private
    fClosed:  Integer;
    fSignature: TSignature;
  private
    procedure DoUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create(const aSignature: TSignature); reintroduce;
  public
    function IsClosed: Boolean;
    procedure Close;
  end;


implementation

const
  WM_COUNT = WM_USER + 1;


type
  TVersion = packed record
    X1: Byte;
    X2: Byte;
    X3: Byte;
  end;

  TDate = packed record
    Day: Byte;
    Month: Byte;
    Year: Word;
  end;


{$REGION Signature}

{ TThreadSignature }

procedure TThreadSignature.DoUpdate;
begin
  fSignature.fOnUpdate(fSignature);
end;

procedure TThreadSignature.Execute;
var
  Res: Boolean;
begin
  Windows.InterLockedExchange(fClosed, 0);
  try

  if fSignature.fInput <> nil then
  begin
    // Пока не отправлен запрос
    while not fSignature.fController.InQueue(fSignature.fInput) do
    begin
      Sleep(20);
      if IsClosed then
        Exit;
    end;
    // Обработка ответа
    Res := fSignature.fInput.Responded;
    if Res then
    begin
      fSignature.fMap.WriteData(Swap(PWord(@fSignature.fInput.RequestPdu^[1])^),
        fSignature.fInput.ResponsePdu);

      if Assigned(fSignature.fOnUpdate) then
        Synchronize(@DoUpdate);
        //fSignature.fOnUpdate(Signature);

      PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
    end
    else
      PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);
  end;

  if fSignature.fHolding <> nil then
  begin

    // Пока не отправлен запрос
    while not fSignature.fController.InQueue(fSignature.fHolding) do
    begin
      Sleep(20);
      if IsClosed then
        Exit;
    end;

    // Обработка ответа
    if fSignature.fHolding.Responded then
    begin
      fSignature.fMap.WriteData(Swap(PWord(@fSignature.fHolding.RequestPdu^[1])^),
        fSignature.fHolding.ResponsePdu);
      PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
    end
    else
      PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);
  end;

  finally
    Windows.InterLockedExchange(fClosed, 1);
  end;

end;

constructor TThreadSignature.Create(const aSignature: TSignature);
begin
  inherited Create(True);
  fSignature := aSignature;
  fClosed := 1;
  FreeOnTerminate := False;
end;

function TThreadSignature.IsClosed: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fClosed, 0, 0) = 1;
end;

procedure TThreadSignature.Close;
begin
  Windows.InterLockedExchange(fClosed, 1);
  WaitForSingleObject(Handle, 5000);
end;

constructor TSignature.Create(const aController: IController;
  const aMap: IMap; const aSlaveId: Byte; const aOnUpdate: TNotifyEvent);
begin
  inherited Create;

  fController := aController;
  fMap     := aMap;
  fSlaveId := aSlaveId;

  fInput   := nil;
  fHolding := nil;

  fOnUpdate := aOnUpdate;

end;

procedure TSignature.AfterConstruction;
begin
  inherited AfterConstruction;
  {$IFDEF DEBUG}
  Assert(fController <> nil);
  Assert(fMap <> nil);
  Assert(fSlaveId <> 0);
  {$ENDIF}

  CreateFrames;

  fThreadSignature := TThreadSignature.Create(Self);
  fThreadSignature.Start;
end;

procedure TSignature.CreateFrames;
begin
  fInput    := ReadInput(fSlaveId, 0, 1, GetSetting.Timeout);
end;

procedure TSignature.BeforeDestruction;
begin
  inherited BeforeDestruction;

  if fThreadSignature <> nil then
  begin
     fThreadSignature.Close;
     fThreadSignature.Free;
     fThreadSignature := nil;
  end;

end;

function TSignature.GetDateIdHard: String;
begin
  Result := 'нет данных';
end;

function TSignature.GetModificator: Byte;
begin
  Result := 0;
end;

function TSignature.GetSerialNumber: String;
begin
  Result := 'нет';
end;

function TSignature.GetTypeFirmWare: Word;
begin
  Result := 0;
end;

function TSignature.GetTypeHard: Word;
begin
  Result := 0;
end;

function TSignature.GetTypeIdHard: Word;
begin
  Result := 0;
end;

function TSignature.GetTypeProject: Word;
begin
  Result := 0;
end;

function TSignature.GetVersionFirmWare: String;
begin
  Result := 'нет';
end;

function TSignature.GetVersionProject: String;
begin
  Result := 'нет';
end;

{$ENDREGION Signature}

{$REGION Auto}
procedure TAuto.CreateFrames;
const
  ADDRESS_INPUT      = 65000;
  NUMBER_INPUT: Word = 15;

  ADDRESS_HOLDING      = 65000;
  NUMBER_HOLDING: Word = 25;
var
  T: Integer;
begin
  T := GetSetting.Timeout;
  fInput    := ReadInput(fSlaveId, ADDRESS_INPUT, NUMBER_INPUT, T);
  fHolding  := ReadHolding(fSlaveId, ADDRESS_HOLDING, NUMBER_HOLDING, T);
end;

function TAuto.GetDateIdHard: String;
var
  Date: TDate;
  WDate: Word;
begin

  WDate := fMap.ReadUint16(TTypeTable.ttHolding, 65009);
  Date.Day := Low(WDate);
  Date.Month := Hi(WDate);
  Date.Year := fMap.ReadUint16(TTypeTable.ttHolding, 65010);
  Result := Format('%.2d.%.2d.%.4d', [Date.Day, Date.Month, Date.Year]);

end;

function TAuto.GetModificator: Byte;
begin
  Result := GetTypeIdHard and $F;
end;

function TAuto.GetSerialNumber: String;
var
  Sn: array[0..17] of Char;
  I, J: Integer;
  W: word;
begin
  J := 0;
  for I := 0 to 8 do
  begin
    W := fMap.ReadUint16(TTypeTable.ttInput, 65001 + I);
    Sn[J] := Chr(Lo(W));  Inc(J);
    Sn[J] := Chr(Hi(W));  Inc(J);
  end;
  Result := String(Sn);
end;

function TAuto.GetTypeFirmWare: Word;
begin
  Result := fMap.ReadUint16(TTypeTable.ttInput, 65011);
end;

function TAuto.GetTypeHard: Word;
begin
  Result := GetTypeIdHard and $FFF0 shr 4;
end;

function TAuto.GetTypeIdHard: Word;
begin
  Result :=  fMap.ReadUint16(TTypeTable.ttInput, 65000);
end;

function TAuto.GetTypeProject: Word;
begin
  Result :=  fMap.ReadUint16(TTypeTable.ttInput, 65013);
end;

function TAuto.GetVersionFirmWare: String;
var
  Version: TVersion;
  V: Word;
begin
  V := fMap.ReadUint16(TTypeTable.ttInput, 65012);
  Version.X1 := V shr 13;
  Version.X2 := (V shr 5) and $7F;
  Version.X3 := V and $F;
  Result := Format('%d.%d.%d', [Version.X1, Version.X2, Version.X3]);
end;

function TAuto.GetVersionProject: String;
var
  Version: TVersion;
  V: Word;
begin
  V := fMap.ReadUint16(TTypeTable.ttInput, 65014);
  Version.X1 := V shr 13;
  Version.X2 := (V shr 5) and $7F;
  Version.X3 := V and $F;
  Result := Format('%d.%d.%d', [Version.X1, Version.X2, Version.X3]);
end;

{$ENDREGION Auto}

{$REGION RCCU}
procedure TRCCU.CreateFrames;
const
  ADDRESS_INPUT = 0;
  NUMBER_INPUT: Word = 3;

  ADDRESS_HOLDING = 173;
  NUMBER_HOLDING: Word = 3;

begin
  fInput    := ReadInput(fSlaveId, ADDRESS_INPUT, NUMBER_INPUT, GetSetting.Timeout);
  fHolding  := ReadHolding(fSlaveId, ADDRESS_HOLDING, NUMBER_HOLDING, GetSetting.Timeout);
end;

function TRCCU.GetDateIdHard: String;
var
  Date: TDate;
  WDate: Word;
begin
  WDate := fMap.ReadUint16(TTypeTable.ttHolding, 173);
  Date.Day := Low(WDate);
  Date.Month := Hi(WDate);
  Date.Year := fMap.ReadUint16(TTypeTable.ttHolding, 174);
  Result := Format('%.2d.%.2d.%.4d', [Date.Day, Date.Month, Date.Year]);
end;

function TRCCU.GetSerialNumber: String;
var
  Sn: word;
begin
  case GetTypeIdHard of
    $FF: Result := 'нет';
  else
    begin
      Sn := fMap.ReadUint16(TTypeTable.ttHolding, 175);
      Result := IntToStr(Sn);
    end;
  end;
end;

function TRCCU.GetTypeHard: Word;
begin
  Result := Lo(fMap.ReadUint16(TTypeTable.ttInput, 0));
end;

function TRCCU.GetTypeIdHard: Word;
begin
  Result := Hi(fMap.ReadUint16(TTypeTable.ttInput, 0));
end;

function TRCCU.GetVersionFirmWare: String;
var
  V: Word;
begin
  V := fMap.ReadUint16(TTypeTable.ttInput, 1);
  Result := Format('%d.%d', [Hi(V), Lo(V)]);
end;

{$ENDREGION RCCU}

end.
