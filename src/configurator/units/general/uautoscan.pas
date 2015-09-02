unit uAutoScan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Forms, VirtualTrees,
  uModbus,
  uLibrary;
type

  { TAutoScan }

  TAutoScan = class
  private
     fModbusData: TObject;
     fScanning: Integer;
     fThreadId:     TThreadId;
  public
    constructor Create(const aModbusData: TObject);
    destructor Destroy; override;
  public
    procedure StartScan(Sender: TObject);
    procedure StopScan(Sender: TObject);
    function IsScanning: Boolean;

  end;
implementation

uses
  uConfiguratorData,
  uSetting;

function ThreadFnc(aParameter: Pointer): Integer;
const
  WM_COUNT = WM_USER + 1;
  TYPE_FIRMWARE = 65011;
  MIN_ADDRESS = 1;
  MAX_ADDRESS = 246;
var
  ModbusData: TModbusData;
  AutoScan: TAutoScan;
  Frame: IFrame;
  I: Integer;
begin
  Result := 0;

  AutoScan := TAutoScan(aParameter);
  if Autoscan.fModbusData is TModbusData then
    ModbusData := TModbusData(Autoscan.fModbusData)
  else
    Exit;

  // Установка флага сканирования
  Windows.InterLockedExchange(AutoScan.fScanning, 1);
  try

    for I := MIN_ADDRESS to MAX_ADDRESS do
    begin
      // Создаем запрос для чтения регистра
      Frame := ReadInput(I, TYPE_FIRMWARE, 1, GetSetting.Timeout);

      // Пока не отправлен запрос - ждем или выходим
      repeat

        // Досрочное прекращение сканирования
        if not AutoScan.IsScanning then
          Exit;

        Sleep(20);
      // Условие выхода из цикла - помещение запроса в очередь
      until ModbusData.Controller.InQueue(Frame);

      // Обработка ответа
      if Frame.Responded then
      begin

        // Добавить устройство
        ModbusData.AddDevice(I, nil);

        // Отправка сообщения счетчику запросов главного окна
        PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
      end
      else
        PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);

      // Досрочное прекращение сканирования
      if not AutoScan.IsScanning then
        Exit;
    end;
  finally
    // Сброс флага сканирования
    Windows.InterLockedExchange(AutoScan.fScanning, 0);
  end;
end;


{ TAutoScan }

constructor TAutoScan.Create(const aModbusData: TObject);
begin
  inherited Create;
  fThreadId := 0;
  fScanning := 0;
  fModbusData := aModbusData;
end;

destructor TAutoScan.Destroy;
begin
  StopScan(nil);
  inherited Destroy;
end;

procedure TAutoScan.StartScan(Sender: TObject);
begin
  if (fThreadId = 0) then
    fThreadId := BeginThread(@ThreadFnc, Self);
end;

procedure TAutoScan.StopScan(Sender: TObject);
begin
  if fThreadId <> 0 then
  begin
    Windows.InterLockedExchange(fScanning, 0);
    WaitForThreadTerminate(fThreadId, 1000);
    CloseThread(fThreadId);
    fThreadId := 0;
  end;
end;

function TAutoScan.IsScanning: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fScanning, 0, 0) = 1;
end;


end.

