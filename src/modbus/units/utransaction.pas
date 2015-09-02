unit uTransaction;

{$mode objfpc}{$H+}
//{$DEFINE DEBUG}// Альтернатива -dDEBUG
{$C+}// Альтернатива Включить Assert

interface

uses
  Classes, SysUtils,
  Windows,
  uBase,
  uModbus,
  uLogger;

type

  { TTransaction }
  {
    Абстрактный класс
  }
  TTransaction = class(TBase, ITransaction)
    type

    // Основные состояния конечного детерминированного автомата
    TState = (
      sConnect,         // Установить соединение (Начальное)
      sRequest,         // Подготовить запрос
      sWaitForSent,     // Дождаться отправки
      sWaitForResponse, // Дождаться ответа устройства
      sResponse,        // Обработать ответ
      sFinal            // Выход (конечное)
    );

    // Прототип функции перехода
    TStateTransitionFunction =
    procedure(out aState: TState) of object;
    // Таблица переходов (без учета конечного состояния)
    TStateTransitionTable = array [Low(TState) .. Pred(High(TState))]
    of TStateTransitionFunction;

  private
    // Таблица переходов
    fStateTransitionTable: TStateTransitionTable;

    // Указатель на интерфейс
    fCommunication: ICommunication;

    // Указатель на modbus-пакет
    fFrame: IFrame;

  private

  protected
    function GetCommunication: ICommunication;
    procedure SetCommunication(const aCommunication: ICommunication);

    // Функции перехода
    procedure Connect(out aState: TState);
    procedure WaitForSent(out aState: TState);
    procedure WaitForResponse(out aState: TState);

    // Функции перехода должны быть переопределены в потомках
    procedure SetRequest(out aState: TState); virtual;
    procedure GetResponse(out aState: TState); virtual;
  public
    procedure AfterConstruction; override;

  public
    procedure Transact(const aFrame: IFrame);
    property Communication: ICommunication read GetCommunication write SetCommunication;

  end;

  { TRtuTransaction }

  TRtuTransaction = class(TTransaction)
  protected
    // Функции перехода должныпереопределены в потомках
    procedure SetRequest(out {%H-}aState: TState); override;
    procedure GetResponse(out {%H-}aState: TState); override;
  public
    procedure AfterConstruction; override;
  end;

  { TTcpTransaction }

  TTcpTransaction = class(TTransaction)
  protected
    // Функции перехода должны переопределены в потомках
    procedure SetRequest(out {%H-}aState: TState); override;
    procedure GetResponse(out {%H-}aState: TState); override;
  end;


implementation

const

  ILLEGAL_FUNCTION = 1;
  ILLEGAL_DATA_ADDRESS = 2;
  ILLEGAL_DATA_VALUE = 3;
  FAILURE_IN_ASSOCIATED_DEVICE = 4;
  ACKNOWLEDGE = 5;
  BUSY_REJECTED_MESSAGE = 6;
  NEGATIVE_ACKNOWLEDGE = 7;
  MEMORY_PARITY_ERROR = 8;
  GATEWAY_PATH_UNAVAILABLE = 10;
  GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND = 11;

  ERROR_CONNECT     = 12;
  ERROR_WAIT_FOR_SENT = 13;
  ERROR_WAIT_FOR_RESPONSE = 14;
  ERROR_SIZE_RESPONSE = 15;
  ERROR_CHECK_CRC16 = 16;

resourcestring
  sILLEGAL_FUNCTION = 'Illegal Function';
  sILLEGAL_DATA_ADDRESS = 'Illegal Data Address';
  sILLEGAL_DATA_VALUE = 'Illegal Data Value';
  sFAILURE_IN_ASSOCIATED_DEVICE = 'Failure In Associated Device';
  sACKNOWLEDGE = 'Acknowledge';
  sBUSY_REJECTED_MESSAGE = 'Busy, Rejected Message';
  sNEGATIVE_ACKNOWLEDGE = 'NAK – Negative Acknowledgement';
  sMEMORY_PARITY_ERROR = 'Memory Parity Error';
  sGATEWAY_PATH_UNAVAILABLE = 'Gateway Path Unavailable';
  sGATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND = 'Gateway Target Device Failed to respond';

  sERROR_CONNECT     = 'Ошибка соединения';
  sERROR_WAIT_FOR_SENT = 'Ошибка ожидания отправки запроса в устройство';
  sERROR_WAIT_FOR_RESPONSE = 'Ошибка ожидания ответа устройства';
  sERROR_SIZE_RESPONSE = 'Неверный размер фрейма ответа';
  sERROR_CHECK_CRC16 = 'Ошибка CRC16 фрейма ответа';


{ TTransaction }


function TTransaction.GetCommunication: ICommunication;
begin
  Result := fCommunication;
end;

procedure TTransaction.SetCommunication(const aCommunication: ICommunication);
begin
  {$IFDEF DEBUG}
  Assert(aCommunication <> nil);
  {$ENDIF}
  fCommunication := aCommunication;

end;

procedure TTransaction.AfterConstruction;
begin
  inherited AfterConstruction;

  // Карта ошибок
  fErrors.Add(ILLEGAL_FUNCTION, sILLEGAL_FUNCTION);
  fErrors.Add(ILLEGAL_DATA_ADDRESS, sILLEGAL_DATA_ADDRESS);
  fErrors.Add(ILLEGAL_DATA_VALUE, sILLEGAL_DATA_VALUE);
  fErrors.Add(FAILURE_IN_ASSOCIATED_DEVICE, sFAILURE_IN_ASSOCIATED_DEVICE);
  fErrors.Add(ACKNOWLEDGE, sACKNOWLEDGE);
  fErrors.Add(BUSY_REJECTED_MESSAGE, sBUSY_REJECTED_MESSAGE);
  fErrors.Add(NEGATIVE_ACKNOWLEDGE, sNEGATIVE_ACKNOWLEDGE);
  fErrors.Add(MEMORY_PARITY_ERROR, sMEMORY_PARITY_ERROR);
  fErrors.Add(GATEWAY_PATH_UNAVAILABLE, sGATEWAY_PATH_UNAVAILABLE);
  fErrors.Add(GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND,
    sGATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND);

  fErrors.Add(ERROR_CONNECT, sERROR_CONNECT);
  fErrors.Add(ERROR_WAIT_FOR_SENT, sERROR_WAIT_FOR_SENT);
  fErrors.Add(ERROR_WAIT_FOR_RESPONSE, sERROR_WAIT_FOR_RESPONSE);
  fErrors.Add(ERROR_SIZE_RESPONSE, sERROR_SIZE_RESPONSE);

  // Таблица функций переходов
  fStateTransitionTable[TState.sConnect]     := @Connect;
  fStateTransitionTable[TState.sRequest]     := @SetRequest;
  fStateTransitionTable[TState.sWaitForSent] := @WaitForSent;
  fStateTransitionTable[TState.sWaitForResponse] := @WaitForResponse;
  fStateTransitionTable[TState.sResponse]    := @GetResponse;

end;


procedure TTransaction.Transact(const aFrame: IFrame);
var
  State:  TState;
  Logger: TLogger;
  LastError: dword;
begin
  {$IFDEF DEBUG}
  Assert(aPack <> nil);
  {$ENDIF}

  // Инициализация
  fFrame := aFrame;


  // Основной цикл работы конечного автомата
  State := TState.sConnect;
  while (State <> TState.sFinal) do
  begin
    // Процедура возвращает новое состояние автомата
    fStateTransitionTable[State](State);
  end;

 // Завершение транзакции
 LastError := Self.GetLastError;
 if LastError = 0 then
    // Сигнализировать об успешном окончании транзакции
    fFrame.Responded := True
  else
  begin
    // Если есть ошибка - сохранить ее в файл
    Logger := TTsLogger.GetInstance.Lock;
    try
      Logger.LogError(GetErrorDesc(LastError), ToString);
    finally
      TTsLogger.GetInstance.Unlock;
    end;
  end;

end;

procedure TTransaction.Connect(out aState: TState);
begin
  aState := TState.sFinal;

  // Инициализация событий: Сброс событий
  ResetEvent(fCommunication.SentEvent);
  ResetEvent(fCommunication.ReadyReadEvent);

  // Подключение к устройству
  if not Communication.IsConnected then
    Communication.Connect;

  // Проверка связи
  case Communication.IsConnected of
    True: aState      := TState.sRequest;
    False: fLastError := ERROR_CONNECT;
  end;
end;

procedure TTransaction.WaitForSent(out aState: TState);
begin
  // Ожидание отправки запроса в устройство
  aState := TState.sFinal;
  case WaitForSingleObject(fCommunication.SentEvent, fFrame.Timeout) of
    WAIT_OBJECT_0:
      aState := TState.sWaitForResponse;
    else
    begin
      fLastError := ERROR_WAIT_FOR_SENT;
    end;
  end;
end;

procedure TTransaction.WaitForResponse(out aState: TState);
begin
 // Ожидание приема данных
    aState := TState.sFinal;
    case WaitForSingleObject(fCommunication.ReadyReadEvent, fFrame.Timeout) of
      WAIT_OBJECT_0:
        aState := TState.sResponse;
      else
        fLastError := ERROR_WAIT_FOR_RESPONSE;
    end;
end;

procedure TTransaction.SetRequest(out aState: TState);
begin
  aState := TState.sWaitForSent;
  fCommunication.NotifyToSend;
end;

procedure TTransaction.GetResponse(out aState: TState);
begin
  aState := TState.sFinal;
end;

{ TRtuTransaction }

procedure TRtuTransaction.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors.Add(ERROR_CHECK_CRC16, sERROR_CHECK_CRC16);
end;

procedure TRtuTransaction.SetRequest(out aState: TState);
var
  Crc16: Word;
begin
  Crc16 := $FFFF;
  fCommunication.Lock;
  try

    // Создать ADU в буфере обмена

    // Адрес устройства
    fCommunication.Buffer^[0] := fFrame.SlaveId;

    // PDU
    // Данные записываются в Big Indian: Hi - Lo
    System.Move(fFrame.RequestPdu^[0], fCommunication.Buffer^[1],
      fFrame.RequestCount);

    // CRC16
    fCommunication.Count := fFrame.RequestCount + 1 {SlaveId};
    TCrc16.CalcCrc16(fCommunication.Buffer, fCommunication.Count, Crc16);

    // CRC16 записывается в Little Indian: Lo - Hi
    fCommunication.Buffer^[fCommunication.Count] := Lo(Crc16);
    fCommunication.Buffer^[fCommunication.Count + 1] := Hi(Crc16);
    fCommunication.Count := fCommunication.Count + 2;

  finally
    fCommunication.Unlock;
  end;
  inherited SetRequest(aState);
end;

procedure TRtuTransaction.GetResponse(out aState: TState);
var
  Crc16Response, Crc16: Word;
begin
  inherited GetResponse(aState);
  Crc16 := $FFFF;

  fCommunication.Lock;
  try

    // Если размер пакета ответа менее 4 - неправильный размер пакета
    if fCommunication.Count < 4 then
    begin
      fLastError := ERROR_SIZE_RESPONSE;
      Exit;
    end;

    // Сравнить CRC16 в последних 2 байтах и CRC16 по фрейму
    // CRC16 ответа
    Crc16Response := fCommunication.Buffer^[fCommunication.Count - 1] shl
      8 or fCommunication.Buffer^[fCommunication.Count - 2];
    // Расчетное CRC16 ответа
    TCrc16.CalcCrc16(fCommunication.Buffer, fCommunication.Count - 2, Crc16);
    if Crc16Response <> Crc16 then
    begin
      fLastError := ERROR_CHECK_CRC16;
      Exit;
    end;

    // Проверка кода функции
    if fCommunication.Buffer^[0] and $80 = $80 then
    begin
      fLastError := fCommunication.Buffer^[1];
      Exit;
    end;

    // Если все успешно, то сохраняем ответ
    fFrame.ResponseCount := fCommunication.Count - 3;
    System.Move(fCommunication.Buffer^[1], fFrame.ResponsePdu^[0],
      fFrame.ResponseCount);


  finally
    fCommunication.Unlock;
  end;
end;


{ TTcpTransaction }

procedure TTcpTransaction.SetRequest(out aState: TState);
const
  MBAP_HEADER_SIZE = 7;
var
  Length: Word;
  {$IFDEF DEBUG}
  I: Integer;
  {$ENDIF}
begin
  fCommunication.Lock;
  try


    {$IFDEF DEBUG}
    writeln('---------Request--------- ');

    writeln(' ');
    writeln('Slave = ', fPack.SlaveId, ', Count = ', fPack.RequestCount);
    for I := 0 to fPack.RequestCount - 1 do
      write(fPack.RequestBuffer^[I], ' ');
    writeln(' ');
    writeln('--------End Request---------- ');
    {$ENDIF}

    // MPBAP заголовок предназначен для определения в стеке
    // протоколов TCP/IP протокол MODBUS TCP/IP
    // Идентификатор транзакции: 2 bytes всегда 00 00
    fCommunication.Buffer^[0] := 0;
    fCommunication.Buffer^[1] := 0;
    // Идентификатор протокола: 2 bytes всегда 00 00
    fCommunication.Buffer^[2] := 0;
    fCommunication.Buffer^[3] := 0;
    // Количество последующих байт: 2 bytes
    Length := {размер адреса устройства}SizeOf(Byte) +
      {количество данных}fFrame.RequestCount;
    fCommunication.Buffer^[4] := Hi(Length);
    fCommunication.Buffer^[5] := Lo(Length);
    // Идентификатор устройства: 1 byte
    fCommunication.Buffer^[6] := fFrame.SlaveId;

    // Записать данные в буфер
    System.Move(fFrame.RequestPdu^[0], fCommunication.Buffer^[MBAP_HEADER_SIZE],
      fFrame.RequestCount);

    // Размер данных для отправки
    fCommunication.Count := MBAP_HEADER_SIZE + fFrame.RequestCount;


  finally
    fCommunication.Unlock;
  end;
  inherited SetRequest(aState);
end;

procedure TTcpTransaction.GetResponse(out aState: TState);
const
  MBAP_HEADER_SIZE = 7;
{$IFDEF DEBUG}
var
  I: Integer;
{$ENDIF}
begin

  inherited GetResponse(aState);

  fCommunication.Lock;
  try
    // Если размер пакета ответа менее 10 - неправильный размер пакета
    if fCommunication.Count < 9 then
    begin
      fLastError := ERROR_SIZE_RESPONSE;
      Exit;
    end;

    // Проверка кода функции
    if fCommunication.Buffer^[MBAP_HEADER_SIZE] and $80 = $80 then
    begin
      fLastError := fCommunication.Buffer^[8];
      Exit;
    end;

    // Если все успешно - сохраняем ответ
    // Количество байт для чтения
    fFrame.ResponseCount := (fCommunication.Buffer^[4] shl 8 or
      fCommunication.Buffer^[5]) - 1 {SlaveId};


    System.Move(fCommunication.Buffer^[MBAP_HEADER_SIZE], fFrame.ResponsePdu^[0],
      fFrame.ResponseCount);

    {$IFDEF DEBUG}
    writeln('---------Response--------- ');

    writeln(' ');
    writeln('Slave = ', fPack.SlaveId, ', Count = ', fPack.ResponseCount);

    for I := 0 to fPack.ResponseCount - 1 do
      write(fPack.ResponseBuffer^[I], ' ');
    writeln(' ');
    writeln('--------End Response---------- ');
    {$ENDIF}

  finally
    fCommunication.Unlock;
  end;



end;

end.
