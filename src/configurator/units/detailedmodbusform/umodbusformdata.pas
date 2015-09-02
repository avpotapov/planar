// Редактирование текущих настроек контроллера
unit uModbusFormData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uCustomEditor, upropertyeditor,
  uConfiguratorData,
  uModbus;

type

{ TModbusBaseData }

TModbusBaseData = class(TPropertyData)
private
  fModbusData: TModbusData;
  fReadOnly: Boolean;
public
  constructor Create(const aCaption: string; const aModbusData: TModbusData); reintroduce;
  property ReadOnly: Boolean read fReadOnly write fReadOnly;
end;


{ TCommaTextData }

TCommaTextData = class(TModbusBaseData)
public
  constructor Create(const aCaption: String; const aCommaText: String; const aModbusData: TModbusData); reintroduce;
end;

{ TBaudRate }

TBaudRate = class(TCommaTextData)
protected
  procedure SetValue(const aValue: string); override;
  function GetValue: string;  override;
public
  constructor Create(const aModbusData: TModbusData); reintroduce;
end;

{ TParity }

TParity = class(TCommaTextData)
protected
  procedure SetValue(const aValue: string); override;
  function GetValue: string;  override;
public
  constructor Create(const aModbusData: TModbusData); reintroduce;
end;

{ TStopBits }

TStopBits = class(TCommaTextData)
protected
  procedure SetValue(const aValue: string); override;
  function GetValue: string;  override;
public
  constructor Create(const aModbusData: TModbusData); reintroduce;
end;

{ TByteSize }

TByteSize = class(TCommaTextData)
protected
  procedure SetValue(const aValue: string); override;
  function GetValue: string;  override;
public
  constructor Create(const aModbusData: TModbusData); reintroduce;
end;

{ TThreeAndHalf }

TThreeAndHalf = class(TModbusBaseData)
protected
  procedure SetValue(const aValue: string); override;
  function GetValue: string;  override;
public
  constructor Create(const aModbusData: TModbusData); reintroduce;
end;

{ TIpAddress }

TIpAddress = class(TModbusBaseData)
protected
 procedure SetValue(const aValue: string); override;
 function GetValue: string;  override;
public
 constructor Create(const aModbusData: TModbusData); reintroduce;
end;

{ TPort }

TPort = class(TModbusBaseData)
protected
 procedure SetValue(const aValue: string); override;
 function GetValue: string;  override;
public
 constructor Create(const aModbusData: TModbusData); reintroduce;
end;


implementation

{ TModbusBaseData }

constructor TModbusBaseData.Create(const aCaption: string;
  const aModbusData: TModbusData);
begin
  inherited Create(aCaption);
  fModbusData := aModbusData;
  fReadOnly := False;
end;

{ TCommaTextData }

constructor TCommaTextData.Create(const aCaption: String;
  const aCommaText: String; const aModbusData: TModbusData);
begin
  inherited Create(aCaption, aModbusData);
  TypeEditor := TTypeEditor.teComboBox;
  Strings.CommaText := aCommaText;
end;

{ TPort }

constructor TPort.Create(const aModbusData: TModbusData);
begin
  inherited Create('Порт', aModbusData);
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

function TPort.GetValue: string;
var
  Port: Word;
begin
  Port := (fModbusData.Controller.Transaction.Communication as ITcpCommunication).TcpConnection.Port;
  Result := IntToStr(Port);
end;

procedure TPort.SetValue(const aValue: string);
var
  Port: word;
  Flag: Boolean;
begin
  Port := StrToInt(aValue);

  Flag := fModbusData.Controller.IsOpen;

  if Flag then
     fModbusData.Controller.Close;

  (fModbusData.Controller.Transaction.Communication as ITcpCommunication).TcpConnection.Port := Port;

  if Flag then
     fModbusData.Controller.Open;
end;

{ TIpAddress }
constructor TIpAddress.Create(const aModbusData: TModbusData);
begin
  inherited Create('IP-адрес', aModbusData);
  TypeEditor := TBaseData.TTypeEditor.teEdit;
end;

function TIpAddress.GetValue: string;
begin
  Result := (fModbusData.Controller.Transaction.Communication as ITcpCommunication).TcpConnection.Ip;
end;

procedure TIpAddress.SetValue(const aValue: string);
var
  Flag: Boolean;
begin
  Flag := fModbusData.Controller.IsOpen;

  if Flag then
     fModbusData.Controller.Close;

  (fModbusData.Controller.Transaction.Communication as ITcpCommunication).TcpConnection.Ip := aValue;

  if Flag then
     fModbusData.Controller.Open;
end;

{ TThreeAndHalf }

constructor TThreeAndHalf.Create(const aModbusData: TModbusData);
begin
  inherited Create('Таймаут в 3,5 байта',  aModbusData);
  TypeEditor := TTypeEditor.teSpinEdit;
end;

function TThreeAndHalf.GetValue: string;
var
  ThreeAndHalf: Dword;
begin
   ThreeAndHalf := (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.ThreeAndHalf;
   Result := IntToStr(ThreeAndHalf);
end;

procedure TThreeAndHalf.SetValue(const aValue: string);
var
  ThreeAndHalf: Integer;
  Flag: Boolean;
begin
  ThreeAndHalf := StrToInt(aValue);

  Flag := fModbusData.Controller.IsOpen;

  if Flag then
     fModbusData.Controller.Close;

  (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.ThreeAndHalf := ThreeAndHalf;

  if Flag then
     fModbusData.Controller.Open;
end;

{ TByteSize }

constructor TByteSize.Create(const aModbusData: TModbusData);
begin
  inherited Create(
    'Число бит в байте',
    '6,7,8',
    aModbusData);
end;

function TByteSize.GetValue: string;
var
  ByteSize: Dword;
begin
   ByteSize := (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.ByteSize;
   Result := IntToStr(ByteSize);
end;

procedure TByteSize.SetValue(const aValue: string);
var
  ByteSize: Dword;
  Flag: Boolean;
begin
  ByteSize := StrToInt(aValue);

  Flag := fModbusData.Controller.IsOpen;

  if Flag then
     fModbusData.Controller.Close;

  (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.ByteSize := ByteSize;

  if Flag then
     fModbusData.Controller.Open;
end;

{ TStopBits }

constructor TStopBits.Create(const aModbusData: TModbusData);
begin
  inherited Create(
    'Количество стоповых бит',
    '0,1,2',
    aModbusData);
end;

function TStopBits.GetValue: string;
var
  StopBits: Dword;
begin
   StopBits := (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.StopBits;
   Result := IntToStr(StopBits);
end;

procedure TStopBits.SetValue(const aValue: string);
var
  StopBits: Dword;
  Flag: Boolean;
begin
  StopBits := StrToInt(aValue);

  Flag := fModbusData.Controller.IsOpen;

  if Flag then
     fModbusData.Controller.Close;

  (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.StopBits := StopBits;

  if Flag then
     fModbusData.Controller.Open;
end;

{ TParity }
constructor TParity.Create(const aModbusData: TModbusData);
begin
 inherited Create(
   'Режим проверки четности',
   '0,1,2,3',
   aModBusData);
end;

function TParity.GetValue: string;
var
  Parity: Dword;
begin
   Parity := (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.Parity;
   Result := IntToStr(Parity);
end;


procedure TParity.SetValue(const aValue: string);
var
  Parity: Dword;
  Flag: Boolean;
begin
  Parity := StrToInt(aValue);

  Flag := fModbusData.Controller.IsOpen;

  if Flag then
     fModbusData.Controller.Close;

  (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.Parity := Parity;

  if Flag then
     fModbusData.Controller.Open;
end;

{ TBaudRate }

constructor TBaudRate.Create(const aModbusData: TModbusData);
begin
 inherited Create(
    'Скорость передачи данных',
    '1200,2400,4800,9600,14400,19200,38400,56000,57600,115200,128000,256000',
    aModbusData);
end;

function TBaudRate.GetValue: string;
var
  BaudRate: Dword;
begin
   BaudRate := (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.BaudRate;
   Result := IntToStr(BaudRate);
end;


procedure TBaudRate.SetValue(const aValue: string);
var
  BaudRate: Dword;
  Flag: Boolean;
begin
  BaudRate := StrToInt(aValue);

  Flag := fModbusData.Controller.IsOpen;

  if Flag then
     fModbusData.Controller.Close;

  (fModbusData.Controller.Transaction.Communication as IRtuCommunication).RtuConnection.BaudRate := BaudRate;

  if Flag then
     fModbusData.Controller.Open;
end;

end.

