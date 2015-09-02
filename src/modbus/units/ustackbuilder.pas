{

  НАЗНАЧЕНИЕ:
  Создание объектов, участвующих в реализации протокола Modbus
  ЗАВИСИМОСТИ:
    - uBase
    - uModbus
  ПАТТЕРН:
    - Builder
    - Lazy Initialization

}

unit uStackBuilder;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}// Альтернатива -dDEBUG
{$C+}// Альтернатива Включить Assert


interface

uses
  Classes, SysUtils,
  WinSock2,
  uBase,
  uModbus, uCommunication, uConnection, uController, uTransaction;

type

  // Конкретные строители
  TRtuBuilder = class;
  TCnTcpBuilder = class;
  TAcTcpBuilder = class;

  { TStackBuilder }
  {

    НАЗНАЧЕНИЕ:
      Возвращает конкретный Builder из коллекции.

    ОПИСАНИЕ:
      Потокобезопасный контейнер Modbus Builder'ов
      Используется Lazy Initialization: если требуемого Builder'а нет
      в коллекции, он создается, размещается в коллекцию

  }
  TStackBuilderSpec = specialize TThreadSafeWrapper<TBuildersMap>;
  TStackBuilder = class(TStackBuilderSpec, IStackBuilder)
  private
    fCapacity: byte;
  protected
    function GetRtuBuilder: IRtuBuilder;
    function GetCnTcpBuilder: ICnTcpBuilder;
    function GetAcTcpBuilder: IAcTcpBuilder;

  public
    constructor Create(const aCapacity: byte = 5);
    destructor Destroy; override;

  public
    property RtuBuilder: IRtuBuilder read GetRtuBuilder;
    property CnTcpBuilder: ICnTcpBuilder read GetCnTcpBuilder;
    property AcTcpBuilder: IAcTcpBuilder
      read GetAcTcpBuilder;
  end;

  TBuilder = class(TBase, IBuilder)
  protected
    fCapacity: byte;

    fController: IController;

  protected
    procedure BuildController;  virtual;
    procedure BuildTransaction; virtual; abstract;
    procedure BuildCommunication; virtual; abstract;
    procedure BuildConnection; virtual; abstract;

    procedure Build;

  public
    constructor Create(const aCapacity: byte); reintroduce;
  end;


  { TRtuBuilder }

  TRtuBuilder = class(TBuilder, IRtuBuilder)
  private
//    fRtuMbController: IRtuMbController;

    fPortName: PChar;
    fBaudRate: dword;
    fParity: byte;
    fStopBits: byte;
    fByteSize: byte;
    fThreeAndHalf: byte;

  protected
    procedure BuildController;  override;
    procedure BuildTransaction; override;
    procedure BuildCommunication; override;
    procedure BuildConnection; override;

  public
    function GetController(
      const aPortName: PChar;
      const aBaudRate: dword;
      const aParity: byte;
      const aStopBits: byte;
      const aByteSize: byte;
      const aThreeAndHalf: byte
    ): IController;
  end;

  { TCnTcpBuilder }

  TCnTcpBuilder = class(TBuilder, ICnTcpBuilder)
  private
    fIp: string;
    fPort: word;

  protected
    procedure BuildController;  override;
    procedure BuildTransaction; override;
    procedure BuildCommunication; override;
    procedure BuildConnection; override;

  public
      function GetController(
      const aIp: string;
      const aPort: word
    ): IController;
  end;

  { TAcTcpBuilder }

  TAcTcpBuilder = class(TBuilder, IAcTcpBuilder)
  private
    fSocket: TSocket;

  protected
    procedure BuildController;  override;
    procedure BuildTransaction; override;
    procedure BuildCommunication; override;
    procedure BuildConnection; override;

  public
    function GetController(
      const aSocket: TSocket
    ): IController;
  end;


implementation


{ TStackBuilder }
constructor TStackBuilder.Create(const aCapacity: byte);
begin
  inherited Create;
  fT := TBuildersMap.Create;
  fCapacity := aCapacity;
end;

destructor TStackBuilder.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fT);
end;


function TStackBuilder.GetRtuBuilder: IRtuBuilder;
var
  Index: Integer;
  BuildersMap: TBuildersMap;
  {$IFDEF DEBUG}
   _RtuBuilder: IRtuBuilder;
  {$ENDIF}
begin
   BuildersMap := Lock;
  {$IFDEF DEBUG}
   Assert(BuildersMap <> nil);
  {$ENDIF}
  try

    if not BuildersMap.Find(TTypeController.mbRtu, Index) then
      Index := BuildersMap.Add(TTypeController.mbRtu, TRtuBuilder.Create(fCapacity));

    {$IFDEF DEBUG}
    Assert(BuildersMap.Data[Index] <> nil);
    Assert(Supports(BuildersMap.Data[Index], IRtuBuilder, _RtuBuilder));
    {$ENDIF}

    Result :=  BuildersMap.Data[Index] as IRtuBuilder;

  finally
    Unlock;
  end;
end;

function TStackBuilder.GetCnTcpBuilder: ICnTcpBuilder;
var
  Index: Integer;
  BuildersMap: TBuildersMap;
  {$IFDEF DEBUG}
   _CnTcpBuilder: ICnTcpBuilder;
  {$ENDIF}
begin
  BuildersMap := Lock;
 {$IFDEF DEBUG}
  Assert(BuildersMap <> nil);
 {$ENDIF}
 try

   if not BuildersMap.Find(TTypeController.mbCnTcp, Index) then
     Index := BuildersMap.Add(TTypeController.mbCnTcp, TCnTcpBuilder.Create(fCapacity));


   {$IFDEF DEBUG}
   Assert(BuildersMap.Data[Index] <> nil);
   Assert(Supports(BuildersMap.Data[Index], ICnTcpBuilder, _CnTcpBuilder));
   {$ENDIF}

   Result :=  BuildersMap.Data[Index] as ICnTcpBuilder;

 finally
   Unlock;
 end;
end;

function TStackBuilder.GetAcTcpBuilder: IAcTcpBuilder;
var
  Index: Integer;
  BuildersMap: TBuildersMap;
  {$IFDEF DEBUG}
   _AcTcpBuilder: IAcTcpBuilder;
  {$ENDIF}
begin
  BuildersMap := Lock;
 {$IFDEF DEBUG}
  Assert(BuildersMap <> nil);
 {$ENDIF}
 try

   if not BuildersMap.Find(TTypeController.mbAcTcp, Index) then
     Index := BuildersMap.Add(TTypeController.mbAcTcp, TAcTcpBuilder.Create(fCapacity));

   {$IFDEF DEBUG}
   Assert(BuildersMap.Data[Index] <> nil);
   Assert(Supports(BuildersMap.Data[Index], IAcTcpBuilder, _AcTcpBuilder));
   {$ENDIF}

   Result :=  BuildersMap.Data[Index] as IAcTcpBuilder;

 finally
   Unlock;
 end;
end;

{ TBuilder }

constructor TBuilder.Create(const aCapacity: byte);
begin
  inherited Create;
  fCapacity := aCapacity;
end;


procedure TBuilder.BuildController;
begin
  fController := TController.Create(fCapacity);
end;

procedure TBuilder.Build;
begin
  BuildController;
  BuildTransaction;
  BuildCommunication;
  BuildConnection;
end;



{ TRtuBuilder }

function TRtuBuilder.GetController(const aPortName: PChar;
  const aBaudRate: dword; const aParity: byte; const aStopBits: byte;
  const aByteSize: byte; const aThreeAndHalf: byte): IController;
begin
  fPortName := aPortName;
  fBaudRate := aBaudRate;
  fParity := aParity;
  fStopBits := aStopBits;
  fByteSize := aByteSize;
  fThreeAndHalf := aThreeAndHalf;

  Build;

  Result := fController;
end;

procedure TRtuBuilder.BuildController;
begin
  inherited BuildController;
  {$IFDEF DEBUG}
  Assert(fController <> nil);
  {$ENDIF}
  fController.TypeController := TTypeController.mbRtu;
end;

procedure TRtuBuilder.BuildTransaction;
begin
  {$IFDEF DEBUG}
   Assert(fController <> nil);
  {$ENDIF}
  fController.Transaction := TRtuTransaction.Create;
end;

procedure TRtuBuilder.BuildCommunication;
begin
  {$IFDEF DEBUG}
   Assert(fController <> nil);
   Assert(fController.Transaction <> nil);
  {$ENDIF}
  fController.Transaction.Communication := TRtuCommunication.Create;
end;

procedure TRtuBuilder.BuildConnection;
var
  _RtuCommunication: IRtuCommunication;
  RtuStruct: TRtuStruct;
begin
  {$IFDEF DEBUG}
  Assert(fController <> nil);
  Assert(fController.Transaction <> nil);
  Assert(fController.Transaction.Communication <> nil);
  Assert(Supports(fController.Transaction.Communication, IRtuCommunication,
    _RtuCommunication));
  {$ENDIF}
  if Supports(fController.Transaction.Communication, IRtuCommunication,
    _RtuCommunication) then
   begin
     RtuStruct.BaudRate := fBaudRate;
     RtuStruct.ByteSize := fByteSize;
     RtuStruct.Parity := fParity;
     RtuStruct.PortName:= fPortName;
     RtuStruct.StopBits:= fStopBits;
     RtuStruct.ThreeAndHalf:= fThreeAndHalf;

     _RtuCommunication.RtuConnection:= TRtuConnection.Create(@RtuStruct);
   end;
end;

{ TCnTcpBuilder }

function TCnTcpBuilder.GetController(const aIp: string;
  const aPort: word): IController;
begin
  fIp := aIp;
  fPort := aPort;

  Build;
  Result := fController;
end;

procedure TCnTcpBuilder.BuildController;
begin
  inherited BuildController;
  {$IFDEF DEBUG}
  Assert(fController <> nil);
  {$ENDIF}
  fController.TypeController := TTypeController.mbCnTcp;
end;

procedure TCnTcpBuilder.BuildTransaction;
begin
  {$IFDEF DEBUG}
   Assert(fController <> nil);
  {$ENDIF}
  fController.Transaction := TTcpTransaction.Create;
end;

procedure TCnTcpBuilder.BuildCommunication;
begin
  {$IFDEF DEBUG}
   Assert(fController <> nil);
   Assert(fController.Transaction <> nil);
  {$ENDIF}
  fController.Transaction.Communication := TTcpCommunication.Create;
end;

procedure TCnTcpBuilder.BuildConnection;
var
  _TcpCommunication: ITcpCommunication;
begin
  {$IFDEF DEBUG}
  Assert(fController <> nil);
  Assert(fController.Transaction <> nil);
  Assert(fController.Transaction.Communication <> nil);
  Assert(Supports(fController.Transaction.Communication, ITcpCommunication,
    _TcpCommunication));
  {$ENDIF}
  if Supports(fController.Transaction.Communication, ITcpCommunication,
    _TcpCommunication) then
    _TcpCommunication.TcpConnection:= TCnTcpConnection.Create(fIp, fPort);
end;

{ TAcTcpBuilder }

function TAcTcpBuilder.GetController(const aSocket: TSocket
  ): IController;
begin
  fSocket := aSocket;
  Build;
  Result := fController;
end;

procedure TAcTcpBuilder.BuildController;
begin
  inherited BuildController;
 {$IFDEF DEBUG}
  Assert(fController <> nil);
  {$ENDIF}
  fController.TypeController := TTypeController.mbAcTcp;
end;

procedure TAcTcpBuilder.BuildTransaction;
begin
 {$IFDEF DEBUG}
  Assert(fController <> nil);
 {$ENDIF}
 fController.Transaction := TTcpTransaction.Create;
end;

procedure TAcTcpBuilder.BuildCommunication;
begin
  {$IFDEF DEBUG}
   Assert(fController <> nil);
   Assert(fController.Transaction <> nil);
  {$ENDIF}
  fController.Transaction.Communication := TTcpCommunication.Create;
end;

procedure TAcTcpBuilder.BuildConnection;
var
  _TcpCommunication: ITcpCommunication;
begin
  {$IFDEF DEBUG}
  Assert(fController <> nil);
  Assert(fController.Transaction <> nil);
  Assert(fController.Transaction.Communication <> nil);
  Assert(Supports(fController.Transaction.Communication, ITcpCommunication,
    _TcpCommunication));
  {$ENDIF}
  if Supports(fController.Transaction.Communication, ITcpCommunication,
    _TcpCommunication) then
    _TcpCommunication.TcpConnection:= TAcTcpConnection.Create(fSocket);
end;
end.
