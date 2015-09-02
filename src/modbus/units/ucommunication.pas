{
  Модуль отвечает за непоследственный обмен данных между устройствами

  Каждый объект создает поток событий, который отслеживает изменения как на
  коммуникационном устройстве, так и действия пользователя: отправить данные,
  прервать коммуникационного устройства.

  Имеется потокобезопасный буфер обмена и количество записанных в него байт
  данных.

  Как только в буфере появились данные, с помощью метода SetSend сигнализируется
  о необходимости отправки данных.
  Если данные успешно отправлены, то возникает взводится событие WaitSentEvent.

  Как только в устройстве появились данные для чтения, они записываются в буфер
  обмена.

  Если есть данные в буфере чтения, сигнализиет ожидающему объекту об этом с
  помощью  WaitReadyReadEvent.

  Объекты также создают, закрывают поток событий и соединение с устройством.
  Об активности объекта можно узнать с помощью флага IsConnected
}

unit uCommunication;

{$mode objfpc}{$H+}
//{$DEFINE DEBUG}// Альтернатива -dDEBUG
{$C+}// Альтернатива Включить Assert
interface

uses
  Classes, SysUtils,
  Windows,
  Winsock2,
  uBase,
  uModbus;

type
{$REGION TCommunication}
  { TCommunication }
  {
      Реализует взаимодействие с устройством
  }
  TCommunication = class(TClipboard, ICommunication)
  private
    // Событие ожидания отправки данных на устройство
    fSentEvent:      THandle;
    // Событие ожидания записи во внутренний буфер обмена
    fReadyReadEvent: THandle;
  protected
    // Флаг активности потока: 1 - активен, 0 - нет
    fActive: Integer;

  protected
    procedure DoSend; virtual; abstract;
    procedure DoRecv; virtual; abstract;

    function GetSentEvent: THandle;
    function GetReadyReadEvent: THandle;

  public
    constructor Create;
    destructor Destroy; override;

  public
    // Выполняется на стороне класса-клиента.
    // Сигнализирует о необходимости отправить данные.
    procedure NotifyToSend; virtual; abstract;

    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    function IsConnected: Boolean; virtual;

    // Событие ожидания отправки данных на устройство
    property SentEvent: THandle read GetSentEvent;
    // Событие ожидания записи во внутренний буфер обмена
    property ReadyReadEvent: THandle read GetReadyReadEvent;
  end;

 {$ENDREGION TCommunication}

{$REGION TStubCommunication - класс-заглушка для тестирования контроллера}
  { TStubCommunication }

  TStubCommunication = class(TCommunication)
  protected
    procedure DoSend; override;
    procedure DoRecv; override;
  public
    // Выполняется на стороне класса-клиента.
    // Сигнализируем о необходимости отправить данные.
    procedure NotifyToSend; override;

    procedure Connect; override;
    procedure Disconnect; override;

  end;

{$ENDREGION TStubCommunication - класс-заглушка для тестирования контроллера}

{$REGION TTcpCommunication}
  { TTcpCommunication }

  TTcpCommunication = class(TCommunication, ITcpCommunication)
  private
    fTcpConnection: ITcpConnection;
    fEventLoop: TThreadId;
    // События нити
    //  fEvents[0] используется для остановки нити
    //  fEvents[1] используется для отправки сообщения
    //  fEvents[2] связывается с событиями FD_READ, FD_WRITE и FD_CLOSE
    fEvents: array[0..2] of WSAEvent;

  private
    procedure CreateEvents;
    procedure CloseEvents;
    procedure ResetEvents;

  protected
    function GetTcpConnection: ITcpConnection;
    procedure SetTcpConnection(const aTcpConnection: ITcpConnection);

    procedure DoSend; override;
    procedure DoRecv; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    // Выполняется на стороне класса-клиента.
    // Сигнализирует о необходимости отправить данные.
    procedure NotifyToSend; override;

    procedure Connect; override;
    procedure Disconnect; override;

    property TcpConnection: ITcpConnection read GetTcpConnection write SetTcpConnection;
  end;

{$ENDREGION TTcpCommunication}

{$REGION TRtuCommunication}
  { TRtuCommunication }

  TRtuCommunication = class(TCommunication, IRtuCommunication)
  private
    Mask: dword;
    fRtuConnection: IRtuConnection;
    fEventLoop: TThreadId;
    // События нити
    fBreakEvent: THandle;

    fOverlappedStatus: TOverlapped;
    fOverlappedRead:   TOverlapped;
    fOverlappedWrite:  TOverlapped;
  private
    procedure CreateEvents;
    procedure CloseEvents;
    procedure ResetEvents;

    function WaitForAsync(aOverlapped: POverlapped): dword;

  protected
    function GetRtuConnection: IRtuConnection;
    procedure SetRtuConnection(const aRtuConnection: IRtuConnection);

    procedure DoSend; override;
    procedure DoRecv; override;
    procedure DoStatus;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    procedure Connect; override;
    procedure Disconnect; override;

    procedure NotifyToSend; override;

    property RtuConnection: IRtuConnection read GetRtuConnection write SetRtuConnection;
  end;

{$ENDREGION TTcpCommunication}

implementation

const
  ERROR_WSA_CREATE_EVENT = 1;
  ERROR_THREAD_ARGUMENT = 2;
  ERROR_CREATE_EVENT_LOOP = 3;
  ERROR_WSA_EVENT_SELECT = 4;
  ERROR_ENUM_NETWORK_EVENT = 5;
  ERROR_FD_READ    = 6;
  ERROR_FD_WRITE   = 7;
  ERROR_WSA_WAIT_FUNCTION = 8;
  ERROR_CONNECTION = 9;

  ERROR_WAIT_COMM_EVENT = 10;
  ERROR_CREATE_EVENT    = 11;
  ERROR_WAIT_FUNCTION   = 12;
  ERROR_OVERPLAPPED     = 13;

resourcestring
  sERROR_WSA_CREATE_EVENT = 'Ошибка создания сокетного события';
  sERROR_THREAD_ARGUMENT = 'Аргумент потока равен nil';
  sERROR_CREATE_EVENT_LOOP = 'Ошибка создания потока событий';
  sERROR_WSA_EVENT_SELECT = 'Ошибка привязки сокета к событию';
  sERROR_ENUM_NETWORK_EVENT = 'Ошибка сброса события';
  sERROR_FD_READ      = 'Ошибка в событии FD_READ';
  sERROR_FD_WRITE     = 'Ошибка в событии FD_WRITE';
  sERROR_WSA_WAIT_FUNCTION = 'Ошибка функции ожидания событий';
  sERROR_CONNECTION   = 'Нет соединения';
  sERROR_WAIT_COMM_EVENT = 'Ошибка создания ожидаемого события';
  sERROR_CREATE_EVENT = 'Ошибка создания события';
  sERROR_WAIT_FUNCTION = 'Ошибка ожидания события';
  sERROR_OVERPLAPPED  = 'Ошибка перекрываемой операции';

{$REGION TCommunication}

{ TCommunication }

function TCommunication.GetReadyReadEvent: THandle;
begin
  Result := fReadyReadEvent;
end;

function TCommunication.GetSentEvent: THandle;
begin
  Result := fSentEvent;
end;

constructor TCommunication.Create;
begin
  inherited Create;
  Count := 0;

  fSentEvent      := CreateEvent(nil, False, False, '');
  fReadyReadEvent := CreateEvent(nil, False, False, '');
end;

destructor TCommunication.Destroy;
begin
  CloseHandle(fSentEvent);
  CloseHandle(fReadyReadEvent);

  inherited Destroy;
end;

function TCommunication.IsConnected: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fActive, 0, 0) = 1;
end;

{$ENDREGION TCommunication}

{$REGION TStubCommunication}

{ TStubCommunication }

procedure TStubCommunication.Connect;
begin
  Windows.InterLockedExchange(fActive, 1);
end;

procedure TStubCommunication.Disconnect;
begin
  Windows.InterLockedExchange(fActive, 0);
end;

procedure TStubCommunication.DoSend;
begin
  SetEvent(fSentEvent);
  Sleep(10);
  DoRecv;
end;

procedure TStubCommunication.DoRecv;
begin
  Sleep(10);
  SetEvent(fReadyReadEvent);
end;

procedure TStubCommunication.NotifyToSend;
begin
  DoSend;
end;

{$ENDREGION TStubCommunication}

{$REGION TTcpCommunication}

{ TTcpCommunication }

function TcpEventLoop(Parameter: Pointer): Integer;
var
  TcpCommunication: TTcpCommunication;
  NetEvents: TWSANetworkEvents;
begin
  Result := 0;

  if Parameter = nil then
  begin
    SetLastError(ERROR_THREAD_ARGUMENT);
    Exit;
  end;

  if not (TObject(Parameter) is TTcpCommunication) then
  begin
    SetLastError(ERROR_THREAD_ARGUMENT);
    Exit;
  end;

  TcpCommunication := TTcpCommunication(Parameter);

  // Флаг активности
  Windows.InterLockedExchange(TcpCommunication.fActive, 1);
  try
    repeat
      case WSAWaitForMultipleEvents(3, @TcpCommunication.fEvents,
          False, WSA_INFINITE, False) of

        // Останавливаем нить
        WSA_WAIT_EVENT_0: Break;

        // Сбрасываем событие и отправляем данные
        WSA_WAIT_EVENT_0 + 1:
        begin
          WSAResetEvent(TcpCommunication.fEvents[1]);
          TcpCommunication.DoSend;
        end;

        // Произошло событие, связанное с сокетом
        WSA_WAIT_EVENT_0 + 2:
        begin

          // Cбрасываем его
          if WSAEnumNetworkEvents(TcpCommunication.TcpConnection.Sock,
            TcpCommunication.fEvents[2], @NetEvents) = SOCKET_ERROR then
          begin
            TcpCommunication.SetLastError(ERROR_ENUM_NETWORK_EVENT);
            Break;
          end;

          // Произошло событие - чтение из буфера сокета
          if NetEvents.lNetworkEvents and FD_READ <> 0 then
          begin
            // Есть ли ошибка
            if NetEvents.iErrorCode[FD_READ_BIT] <> 0 then
            begin
              TcpCommunication.SetLastError(ERROR_FD_READ);
              Break;
            end;
            // Читаем данные из буфера сокет в свой буфер
            TcpCommunication.DoRecv;
            Continue;
          end;

          // Сокет готов к передаче данных
          if NetEvents.lNetworkEvents and FD_WRITE <> 0 then
          begin
            if NetEvents.iErrorCode[FD_WRITE_BIT] <> 0 then
            begin
              TcpCommunication.SetLastError(ERROR_FD_WRITE);
              Break;
            end;
            // Отправляем то, что лежит в буфере
            TcpCommunication.DoSend;
            Continue;
          end;

          // Клиент закрыл соединение
          if NetEvents.lNetworkEvents and FD_CLOSE <> 0 then
            Break;

        end;
        else
        begin
          TcpCommunication.SetLastError(ERROR_WSA_WAIT_FUNCTION);
          Break;
        end;
      end;

      Sleep(10);
    until False;
  finally
    Windows.InterLockedExchange(TcpCommunication.fActive, 0);
  end;
end;

procedure TTcpCommunication.AfterConstruction;
begin
  inherited AfterConstruction;

  // Карта ошибок
  fErrors.Add(ERROR_WSA_CREATE_EVENT, sERROR_WSA_CREATE_EVENT);
  fErrors.Add(ERROR_THREAD_ARGUMENT, sERROR_THREAD_ARGUMENT);
  fErrors.Add(ERROR_CREATE_EVENT_LOOP, sERROR_CREATE_EVENT_LOOP);
  fErrors.Add(ERROR_WSA_EVENT_SELECT, sERROR_WSA_EVENT_SELECT);
  fErrors.Add(ERROR_ENUM_NETWORK_EVENT, sERROR_ENUM_NETWORK_EVENT);
  fErrors.Add(ERROR_FD_READ, sERROR_FD_READ);
  fErrors.Add(ERROR_FD_WRITE, sERROR_FD_WRITE);
  fErrors.Add(ERROR_WSA_WAIT_FUNCTION, sERROR_WSA_WAIT_FUNCTION);
  fErrors.Add(ERROR_CONNECTION, sERROR_CONNECTION);

  // Создание событий
  CreateEvents;
end;

destructor TTcpCommunication.Destroy;
begin
  Disconnect;
  CloseEvents;
  inherited Destroy;
end;

procedure TTcpCommunication.Connect;
begin
  {$IFDEF DEBUG}
  Assert(TcpConnection <> nil);
  {$ENDIF}

  // Нельзя, если серверное соединение
  //Disconnect;

  // Открытие соединения
  TcpConnection.Open;
  if TcpConnection.Sock = INVALID_SOCKET then
  begin
    fLastError := ERROR_CONNECTION;
    Exit;
  end;

  // Сброс событий
  ResetEvents;


  // Привязка сокета к событию
  if WSAEventSelect(TcpConnection.Sock, fEvents[2], FD_READ or FD_WRITE or FD_CLOSE) =
    SOCKET_ERROR then
  begin
    fLastError := ERROR_WSA_EVENT_SELECT;
    Exit;
  end;

  // Создание потока
  fEventLoop := BeginThread(@TcpEventLoop, Self);
  if fEventLoop = 0 then
  begin
    fLastError := ERROR_CREATE_EVENT_LOOP;
    Exit;
  end;

  // Для запуска потока
  Sleep(100);

end;

procedure TTcpCommunication.Disconnect;
const
  TIMEOUT_CLOSE = 100;
begin
  // Закрытие потока
  if fEventLoop <> 0 then
  begin
    WSASetEvent(FEvents[0]);
    WaitForThreadTerminate(fEventLoop, TIMEOUT_CLOSE);
    CloseThread(fEventLoop);
    fEventLoop := 0;
  end;

  // Закрытие сокета
  TcpConnection.Close;

end;

procedure TTcpCommunication.CreateEvents;
begin
  // Создаём события
  fEvents[0] := WSACreateEvent;
  if fEvents[0] = WSA_INVALID_HANDLE then
  begin
    fLastError := ERROR_WSA_CREATE_EVENT;
    Exit;
  end;

  fEvents[1] := WSACreateEvent;
  if fEvents[1] = WSA_INVALID_HANDLE then
  begin
    fLastError := ERROR_WSA_CREATE_EVENT;
    Exit;
  end;

  fEvents[2] := WSACreateEvent;
  if fEvents[2] = WSA_INVALID_HANDLE then
  begin
    fLastError := ERROR_WSA_CREATE_EVENT;
    Exit;
  end;

end;

procedure TTcpCommunication.CloseEvents;
begin
  WSACloseEvent(fEvents[0]);
  WSACloseEvent(fEvents[1]);
  WSACloseEvent(fEvents[2]);
end;

procedure TTcpCommunication.ResetEvents;
begin
  WSAResetEvent(fEvents[0]);
  WSAResetEvent(fEvents[1]);
  WSAResetEvent(fEvents[2]);
end;

function TTcpCommunication.GetTcpConnection: ITcpConnection;
begin
  Result := fTcpConnection;
end;

procedure TTcpCommunication.SetTcpConnection(const aTcpConnection: ITcpConnection);
begin
  fTcpConnection := aTcpConnection;
end;

procedure TTcpCommunication.NotifyToSend;
begin
  // Сообщить потоку о необходимости отправки данных
  WSASetEvent(fEvents[1]);
end;

procedure TTcpCommunication.DoSend;
var
  SentRes: Longint;
begin

  Lock;
  try
    SentRes := WinSock2.Send(TcpConnection.Sock, Buffer[0], Count, 0);

    if WSAGetLastError = WSAEWOULDBLOCK then
      SentRes := 0;

    // Информируем о отправки данных
    if (SentRes > 0) then
      SetEvent(fSentEvent);

  finally
    Unlock;
  end;

end;

procedure TTcpCommunication.DoRecv;
begin
  Lock;
  try

    // Копируем данные из буфера сокета в свой буфер
    Count := WinSock2.Recv(TcpConnection.Sock, Buffer, BUFFER_SIZE, 0);
    if WSAGetLastError = WSAEWOULDBLOCK then
      Count := 0;

    // Информируем о наличии данных в буфере
    if (Count > 0) then
      SetEvent(fReadyReadEvent);

  finally
    Unlock;
  end;

end;

{$ENDREGION TTcpCommunication}

{$REGION TRtuCommunication}

{ TRtuCommunication }


function RtuEventLoop(aParameter: Pointer): Integer;
var
  RtuCommunication: TRtuCommunication;
  Events: array [0 .. 1] of THandle;
  Mask:   dword;
begin
  Result := 0;

  Mask := EV_RXCHAR or EV_ERR or EV_BREAK or EV_TXEMPTY;

  if aParameter = nil then
  begin
    SetLastError(ERROR_THREAD_ARGUMENT);
    Exit;
  end;

  if not (TObject(aParameter) is TRtuCommunication) then
  begin
    SetLastError(ERROR_THREAD_ARGUMENT);
    Exit;
  end;

  RtuCommunication := TRtuCommunication(aParameter);

  Events[0] := RtuCommunication.fBreakEvent;
  Events[1] := RtuCommunication.fOverlappedStatus.hEvent;


  // Флаг активности
  Windows.InterLockedExchange(RtuCommunication.fActive, 1);
  try
    repeat
      // Маска события
      if not SetCommMask(RtuCommunication.RtuConnection.Handle, Mask) then
        Break;

      // Ожидаем события и пимещаем их в маску
      if not WaitCommEvent(RtuCommunication.RtuConnection.Handle,
        RtuCommunication.Mask, @RtuCommunication.fOverlappedStatus) then
        if GetLastError <> ERROR_IO_PENDING then
          Break;

      // Ожидаем окончание перекрываемой операции или наступления иных событий
      case WaitForMultipleObjects(2, @Events, False, INFINITE) of

        // Событие: закрытие потока
        WAIT_OBJECT_0:
          Break;

        // Событие: перекрываемая операция
        WAIT_OBJECT_0 + 1:
        begin
          RtuCommunication.DoStatus;
        end

        else
        begin
          RtuCommunication.SetLastError(ERROR_WAIT_FUNCTION);
          Break;
        end;
      end;
    until False;
  finally
    // Флаг активности
    Windows.InterLockedExchange(RtuCommunication.fActive, 0);
  end;

end;

procedure TRtuCommunication.AfterConstruction;
begin
  inherited AfterConstruction;

  fErrors.Add(ERROR_WAIT_COMM_EVENT, sERROR_WAIT_COMM_EVENT);
  fErrors.Add(ERROR_CREATE_EVENT, sERROR_CREATE_EVENT);
  fErrors.Add(ERROR_THREAD_ARGUMENT, sERROR_THREAD_ARGUMENT);
  fErrors.Add(ERROR_WAIT_FUNCTION, sERROR_WAIT_FUNCTION);
  fErrors.Add(ERROR_OVERPLAPPED, sERROR_OVERPLAPPED);
  fErrors.Add(ERROR_CONNECTION, sERROR_CONNECTION);
  fErrors.Add(ERROR_CREATE_EVENT_LOOP, sERROR_CREATE_EVENT_LOOP);

  CreateEvents;
end;

destructor TRtuCommunication.Destroy;
begin
  SetEvent(fBreakEvent);
  Disconnect;
  CloseEvents;

  inherited Destroy;
end;

procedure TRtuCommunication.CreateEvents;
begin

  fBreakEvent := CreateEvent(nil, False, False, '');   // Break;
  if fBreakEvent = INVALID_HANDLE_VALUE then
  begin
    fLastError := ERROR_CREATE_EVENT;
    Exit;
  end;

  // Перекрываемая структура
  FillChar(fOverlappedStatus, SizeOf(fOverlappedStatus), 0);
  fOverlappedStatus.hEvent := CreateEvent(nil, True, False, nil);
  if fOverlappedStatus.hEvent = INVALID_HANDLE_VALUE then
  begin
    fLastError := ERROR_CREATE_EVENT;
    Exit;
  end;


  FillChar(fOverlappedRead, SizeOf(fOverlappedRead), 0);
  fOverlappedRead.hEvent := CreateEvent(nil, True, False, nil);
  if fOverlappedRead.hEvent = INVALID_HANDLE_VALUE then
  begin
    fLastError := ERROR_CREATE_EVENT;
    Exit;
  end;

  FillChar(fOverlappedWrite, SizeOf(fOverlappedWrite), 0);
  fOverlappedWrite.hEvent := CreateEvent(nil, True, False, nil);
  if fOverlappedWrite.hEvent = INVALID_HANDLE_VALUE then
  begin
    fLastError := ERROR_CREATE_EVENT;
    Exit;
  end;

end;

procedure TRtuCommunication.CloseEvents;
begin
  CloseHandle(fBreakEvent);
  CloseHandle(fOverlappedStatus.hEvent);
  CloseHandle(fOverlappedRead.hEvent);
  CloseHandle(fOverlappedWrite.hEvent);
end;

procedure TRtuCommunication.ResetEvents;
begin
  ResetEvent(fBreakEvent);
  ResetEvent(fOverlappedStatus.hEvent);
  ResetEvent(fOverlappedRead.hEvent);
  ResetEvent(fOverlappedWrite.hEvent);
end;

function TRtuCommunication.WaitForAsync(aOverlapped: POverlapped): dword;
var
  Events: array [0 .. 1] of THandle;
begin

  Result    := 0;
  Events[0] := fBreakEvent;
  Events[1] := aOverlapped^.hEvent;

  try
    case WaitForMultipleObjects(2, @Events, False, INFINITE) of

      WAIT_OBJECT_0:
      begin
        SetEvent(fBreakEvent);
      end;

      WAIT_OBJECT_0 + 1:
        if not GetOverlappedResult(RtuConnection.Handle, aOverlapped^,
          Result, False) then
        begin
          fLastError := ERROR_OVERPLAPPED;
          SetEvent(fBreakEvent);
          Result := 0;
        end;
      else
      begin
        SetEvent(fBreakEvent);
        fLastError := ERROR_WAIT_FUNCTION;
      end;
    end;

  finally
    ResetEvent(aOverlapped^.hEvent);
  end;

end;

function TRtuCommunication.GetRtuConnection: IRtuConnection;
begin
  Result := fRtuConnection;
end;

procedure TRtuCommunication.SetRtuConnection(const aRtuConnection: IRtuConnection);
begin
  fRtuConnection := aRtuConnection;
end;

procedure TRtuCommunication.DoStatus;
var
  NumberOfBytesTransaffered: dword;
begin

  NumberOfBytesTransaffered := WaitForAsync(@fOverlappedStatus);

  if NumberOfBytesTransaffered = 0 then
  begin
    fLastError := ERROR_OVERPLAPPED;
    Exit;
  end;

  // Маска событий прерывания связи
  if ((EV_BREAK and Mask) <> 0) or ((EV_ERR and Mask) <> 0) then
  begin
    fLastError := ERROR_CONNECTION;
    SetEvent(fBreakEvent);
    Exit;
  end;


  // Информируем о отправке данных на устройство
  if (EV_TXEMPTY and Mask) <> 0 then
  begin
    SetEvent(fSentEvent);
  end;

  // Маска события: есть данные в приемном буфере
  if (EV_RXCHAR and Mask) <> 0 then
  begin
    DoRecv;
  end;

end;


procedure TRtuCommunication.DoSend;
var
  NumberOfBytesWritten: dword;
begin
  NumberOfBytesWritten := 0;
  Sleep(RtuConnection.ThreeAndHalf);
  try
    if not WriteFile(RtuConnection.Handle, Buffer^[0], Count,
      NumberOfBytesWritten, @fOverlappedWrite) then
      if (Windows.GetLastError() <> ERROR_IO_PENDING) then
      begin
        fLastError := ERROR_OVERPLAPPED;
        Exit;
      end;

    // Ждем окончания перекрываемой операции
    NumberOfBytesWritten := WaitForAsync(@fOverlappedWrite);

    // Информируем о отправке данных на устройство
    //if (Longint(NumberOfBytesWritten) = Count) then
    //  SetEvent(fSentEvent);

  finally
    if not PurgeComm(RtuConnection.Handle, PURGE_TXCLEAR) then
      SetEvent(fBreakEvent);
  end;
end;

procedure TRtuCommunication.DoRecv;
var
  NumberOfBytesRead: dword;
begin
  NumberOfBytesRead := 0;
  try

    if not ReadFile(RtuConnection.Handle, Buffer^[0], BUFFER_SIZE,
      NumberOfBytesRead, @fOverlappedRead) then
      if (Windows.GetLastError() <> ERROR_IO_PENDING) then
      begin
        SetEvent(fBreakEvent);
        fLastError := ERROR_OVERPLAPPED;
        Exit;
      end;

    NumberOfBytesRead := WaitForAsync(@fOverlappedRead);


    // Информируем о наличии данных в буфере
    if (NumberOfBytesRead > 0) then
    begin
      Count := NumberOfBytesRead;
      SetEvent(fReadyReadEvent);
      Exit;
    end;
  finally
    if not PurgeComm(RtuConnection.Handle, PURGE_RXCLEAR) then
      SetEvent(fBreakEvent);
  end;

end;

procedure TRtuCommunication.Connect;
begin
  {$IFDEF DEBUG}
  Assert(RtuConnection <> nil);
  {$ENDIF}

  Disconnect;
  // Сброс событий
  ResetEvents;

  // Открытие соединения
  RtuConnection.Open;
  if RtuConnection.Handle = INVALID_HANDLE_VALUE then
  begin
    fLastError := ERROR_CONNECTION;
    Exit;
  end;

  // Создание потока
  fEventLoop := BeginThread(@RtuEventLoop, Self);
  if fEventLoop = 0 then
  begin
    fLastError := ERROR_CREATE_EVENT_LOOP;
    Exit;
  end;

  // Для запуска потока
  Sleep(100);
end;

procedure TRtuCommunication.Disconnect;
const
  TIMEOUT_CLOSE = 100;
begin

  // Закрытие потока
  if fEventLoop <> 0 then
  begin
    WaitForThreadTerminate(fEventLoop, TIMEOUT_CLOSE);
    CloseThread(fEventLoop);
    fEventLoop := 0;
  end;

  // Закрытие сокета
  RtuConnection.Close;

end;

procedure TRtuCommunication.NotifyToSend;
begin
  DoSend;
end;

{$ENDREGION TRtuCommunication}
end.
