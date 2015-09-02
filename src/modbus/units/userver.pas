unit uServer;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}// Альтернатива -dDEBUG
{$C+}// Альтернатива Включить Assert

interface

uses
  Classes, SysUtils,
  WinSock2,
  Windows,
  uBase, uModbus;

type

  { TServer }

  TServer = class(TBase, IServer)
  private
    fHandle: THandle; // дескриптор окна обработки сообщения (принимает новый контроллер)

    fAcTcpBuilder: IAcTcpBuilder;
    fControllerPool: IControllerPool;

    fTcpConnection: ITcpConnection;

    // События нити
    //  fEvents[0] используется для остановки нити
    //  fEvents[1] связывается с событием FD_ACCEPT
    fEvents: array[0..1] of WSAEvent;

    // Флаг активности потока: 1 - активен, 0 - нет
    fActive: integer;
    // Указатель на поток событий на порту
    fEventLoop: TThreadId;

  private
    // Создание, закрытие, сброс событий на сокете
    procedure CreateEvents;
    procedure CloseEvents;
    procedure ResetEvents;

  protected
    function GetTcpConnection: ITcpConnection;
    procedure SetTcpConnection(const aTcpConnection: ITcpConnection);

    procedure SetAcTcpBuilder(const aAcTcpBuilder: IAcTcpBuilder);
    procedure SetControllerPool(const aControllerPool: IControllerPool);

    function GetHandle: THandle;
    procedure SetHandle(const aHandle: THandle);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    constructor Create(const aAcTcpBuilder: IAcTcpBuilder;
      const aControllerPool: IControllerPool); overload;

    function Start: boolean;
    procedure Stop;

    function IsActive: boolean;

    property AcTcpBuilder: IAcTcpBuilder write SetAcTcpBuilder;
    property ControllerPoll: IControllerPool write SetControllerPool;
    property TcpConnection: ITcpConnection read GetTcpConnection
      write SetTcpConnection;
    property Handle: THandle read GetHandle write SetHandle;
  end;



implementation

const
  ERROR_WSA_CREATE_EVENT = 1;
  ERROR_WSA_EVENT_SELECT = 2;
  ERROR_THREAD_ARGUMENT = 3;
  ERROR_CREATE_EVENT_LOOP = 4;
  ERROR_ENUM_NETWORK_EVENT = 5;
  ERROR_UNKNOWN_EVENT = 6;
  ERROR_FD_ACCEPT = 7;
  ERROR_WSA_WAIT_FUNCTION = 8;
  ERROR_ACCEPT = 9;
  ERROR_CONNECTION = 10;

resourcestring
  sERROR_WSA_CREATE_EVENT = 'Ошибка создания сокетного события';
  sERROR_WSA_EVENT_SELECT = 'Ошибка связвания события с сокетом';
  sERROR_THREAD_ARGUMENT = 'Аргумент потока равен nil';
  sERROR_CREATE_EVENT_LOOP = 'Ошибка создания потока событий';
  sERROR_ENUM_NETWORK_EVENT = 'Ошибка сброса события';
  sERROR_UNKNOWN_EVENT = 'Неизвестное событие';
  sERROR_FD_ACCEPT = 'Ошибка события Accept';
  sERROR_WSA_WAIT_FUNCTION = 'Ошибка функции ожидания событий';
  sERROR_ACCEPT = 'Ошибка подключения клиента';
  sERROR_CONNECTION = 'Ошибка подключения';


  function ListenPort(Parameter: Pointer): integer;
  var
    Server: TServer;
    NetEvents: TWSANetworkEvents;

     // Сокет, созданный для общения с подключившимся клиентом
    ClientSocket: TSocket;
    // Адрес подключившегося клиента
    ClientAddr: TSockAddr;
    ClientAddrLen: LongInt;

    ClientController: IController;

  begin
    Result := 0;

    if Parameter = nil then
    begin
      SetLastError(ERROR_THREAD_ARGUMENT);
      Exit;
    end;

    // Приведение указателя
    Server := TServer(Parameter);

    // Флаг активности
    Windows.InterLockedExchange(Server.fActive, 1);
    try
      // Основной цикл событий
      repeat
        // Ожидание событий
        case WSAWaitForMultipleEvents(2, @Server.fEvents, False, WSA_INFINITE,
          False) of

          // Событие FEvents[0] взведено - закрытие потока событий
          WSA_WAIT_EVENT_0: Break;

          // Событие FEvents[1] взведено - наступление события FD_ACCEPT
          WSA_WAIT_EVENT_0 + 1:
          begin

            // Сбрасываем событие
            if WSAEnumNetworkEvents(Server.TcpConnection.Sock,
              Server.fEvents[1], @NetEvents) = SOCKET_ERROR then
            begin
              Server.SetLastError(ERROR_ENUM_NETWORK_EVENT);
              Break;
            end;

            // Проверка на соответствие событию FD_ACCEPT
            if NetEvents.lNetworkEvents and FD_ACCEPT = 0 then
            begin
              Server.SetLastError(ERROR_UNKNOWN_EVENT);
              Break;
            end;

            // Проверка, не было ли ошибок
            if NetEvents.iErrorCode[FD_ACCEPT_BIT] <> 0 then
            begin
              Server.SetLastError(ERROR_FD_ACCEPT);
              Break;
            end;

            // Создаем сокетное соединение с клиентом
            ClientAddrLen := SizeOf(ClientAddr);
            // Проверяем наличие подключения
            ClientSocket := Accept(Server.TcpConnection.Sock, @ClientAddr,
              ClientAddrLen);
            if ClientSocket = INVALID_SOCKET then
            begin
              Server.SetLastError(ERROR_ACCEPT);
              if WSAGetLastError <> WSAEWOULDBLOCK then
                Continue;
              Break;
            end;

            // Новый контроллер
            if not Server.fControllerPool.ReplaceConnection(ClientSocket) then
            begin
              ClientController := Server.fAcTcpBuilder.GetController(ClientSocket);
              ClientController.Open;
              Server.fControllerPool.Add(ClientController);

              // Отправить сообщение дескриптору окна с указателем на интерфейс контроллера
              SendMessage(Server.Handle, WM_NEW_CLIENT, {%H-}Integer(Pointer(ClientController)), 0);
            end;

          end

          else
          begin
            Server.SetLastError(ERROR_WSA_WAIT_FUNCTION);
            Break;
          end;

        end;
      until False;
    finally
      // Флаг активности
      Windows.InterLockedExchange(Server.fActive, 0);

    end;
  end;

{ TServer }

procedure TServer.AfterConstruction;
begin
  inherited AfterConstruction;

  fErrors.Add(ERROR_WSA_CREATE_EVENT, sERROR_WSA_CREATE_EVENT);
  fErrors.Add(ERROR_WSA_EVENT_SELECT, sERROR_WSA_EVENT_SELECT);
  fErrors.Add(ERROR_THREAD_ARGUMENT, sERROR_THREAD_ARGUMENT);
  fErrors.Add(ERROR_CREATE_EVENT_LOOP, sERROR_CREATE_EVENT_LOOP);
  fErrors.Add(ERROR_ENUM_NETWORK_EVENT, sERROR_ENUM_NETWORK_EVENT);
  fErrors.Add(ERROR_UNKNOWN_EVENT, sERROR_UNKNOWN_EVENT);
  fErrors.Add(ERROR_FD_ACCEPT, sERROR_FD_ACCEPT);
  fErrors.Add(ERROR_WSA_WAIT_FUNCTION, sERROR_WSA_WAIT_FUNCTION);
  fErrors.Add(ERROR_ACCEPT, sERROR_ACCEPT);
  fErrors.Add(ERROR_CONNECTION, sERROR_CONNECTION);

  // Создание событий
  CreateEvents;
end;

destructor TServer.Destroy;
begin
   Stop;
   CloseEvents;
   inherited Destroy;
end;

constructor TServer.Create(const aAcTcpBuilder: IAcTcpBuilder;
  const aControllerPool: IControllerPool);
begin
  inherited Create;
  fAcTcpBuilder := aAcTcpBuilder;
  fControllerPool := aControllerPool;
end;


function TServer.Start: boolean;
begin
  Result := False;

  {$IFDEF DEBUG}
  Assert(TcpConnection <> nil);
  Assert(TcpConnection.Port <> 0);
  Assert(fAcTcpBuilder <> nil);
  Assert(fControllerPool <> nil);
  {$ENDIF}

  // Вначале закрыть сокет
  Stop;

  // Открытие сокета
  if TcpConnection = nil then
  begin
    fLastError := ERROR_CONNECTION;
    Exit;
  end;

  TcpConnection.Open;

  if TcpConnection.Sock = INVALID_SOCKET then
    Exit;

  {$IFDEF DEBUG}
  Assert(fEvents[1] <> WSA_INVALID_HANDLE);
  {$ENDIF}

  // Привязывание сокета к событию
  if WSAEventSelect(TcpConnection.Sock, fEvents[1], FD_ACCEPT) = SOCKET_ERROR then
  begin
    fLastError := ERROR_WSA_EVENT_SELECT;
    Exit;
  end;

  // Создание слушающего потока
  fEventLoop := BeginThread(@ListenPort, Self);
  if fEventLoop = 0 then
  begin
    fLastError := ERROR_CREATE_EVENT_LOOP;
    Exit;
  end;

  Result := True;
end;

procedure TServer.Stop;
const
  TIMEOUT_CLOSE = 5000;
begin
  // Закрытие потока
  if fEventLoop <> 0 then
  begin
    WSASetEvent(FEvents[0]);
    WaitForThreadTerminate(fEventLoop, TIMEOUT_CLOSE);
    CloseThread(fEventLoop);
    fEventLoop := 0;

    // Сброс событий
    ResetEvents;
  end;

  // Закрытие сокета
  if TcpConnection <> nil then
    TcpConnection.Close;
end;


procedure TServer.CreateEvents;
begin
   // События нити
   //  fEvents[0] используется для остановки нити
   fEvents[0] := WSACreateEvent;
   if fEvents[0] = WSA_INVALID_HANDLE then
   begin
     fLastError := ERROR_WSA_CREATE_EVENT;
     Exit;
   end;

   //  fEvents[1] связывается с событием FD_ACCEPT
   fEvents[1] := WSACreateEvent;
   if FEvents[1] = WSA_INVALID_HANDLE then
   begin
     fLastError := ERROR_WSA_CREATE_EVENT;
     Exit;
   end;

end;

procedure TServer.CloseEvents;
begin
   WSACloseEvent(fEvents[0]);
   WSACloseEvent(fEvents[1]);
end;

procedure TServer.ResetEvents;
begin
   WSAResetEvent(fEvents[0]);
   WSAResetEvent(fEvents[1]);
end;

procedure TServer.SetAcTcpBuilder(const aAcTcpBuilder: IAcTcpBuilder);
begin
  fAcTcpBuilder := aAcTcpBuilder;
end;

procedure TServer.SetControllerPool(const aControllerPool: IControllerPool);
begin
  fControllerPool := aControllerPool;
end;

function TServer.GetHandle: THandle;
begin
  Result := fHandle;
end;

procedure TServer.SetHandle(const aHandle: THandle);
begin
  if fHandle <> aHandle then
     fHandle := aHandle;
end;

function TServer.IsActive: boolean;
begin
  Result := Windows.InterlockedCompareExchange(fActive, 0, 0) = 1;
end;

function TServer.GetTcpConnection: ITcpConnection;
begin
  Result := fTcpConnection;
end;

procedure TServer.SetTcpConnection(const aTcpConnection: ITcpConnection);
begin
  fTcpConnection := aTcpConnection;
end;


end.
