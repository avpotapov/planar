unit uConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WinSock2,
  Windows,
  uBase,
  uModbus;

type

  {$REGION Абстрактный класс TCP - соединения, реализующий ITcpConnection}
  { TTcpConnection }

  TTcpConnection = class(TBase, ITcpConnection)
  private
    fSocket: TSocket;
    fIp:     String;
    fIpChar: array[0 .. 1023] of Char;
    fPort:   Word;
  protected
    // Ip
    function GetIp: String;
    procedure SetIp(const aIp: String); virtual; abstract;

    // Порт
    function GetPort: Word;
    procedure SetPort(const aPort: Word); virtual; abstract;

    // Сокет
    function GetSocket: TSocket;

  public
    constructor Create;
    destructor Destroy; override;

  public
    // Процедуры открытия и закрытия сокета
    procedure Open; virtual; abstract;
    procedure Close;

    property Ip: String read GetIp write SetIp;
    property Port: Word read GetPort write SetPort;
    property Sock: TSocket read GetSocket;
  end;

  {$ENDREGION Абстрактный класс TCP - соединения, реализующий ITcpConnection}

  {$REGION Класс TCP - соединения клиента}
  { TCnTcpConnection }

  TCnTcpConnection = class(TTcpConnection)
  protected
    procedure SetIp(const aIp: String); override;
    procedure SetPort(const aPort: Word); override;

  public
    constructor Create(const aIp: String; const aPort: Word); reintroduce;
    procedure AfterConstruction; override;

  public
    procedure Open; override;
  end;

  {$ENDREGION Класс TCP - соединения клиента}

  {$REGION Класс TCP - соединения серверного клиента}
  { TAcTcpConnection }

  TAcTcpConnection = class(TTCpConnection)
  protected
    procedure SetIp(const {%H-}aIp: String); override;
    procedure SetPort(const {%H-}aPort: Word); override;

  public
    constructor Create(const aSocket: TSocket); reintroduce;
    procedure AfterConstruction; override;

  public
    procedure Open; override;
  end;

  {$ENDREGION Класс TCP - соединения серверного клиента}

  {$REGION Класс TCP - соединения сервера}
  { TSvTcpConnection }

  TSvTcpConnection = class(TTcpConnection)
  protected
    procedure SetIp(const {%H-}aIp: String); override;
    procedure SetPort(const aPort: Word); override;

  public
    constructor Create(const aPort: Word); reintroduce;
    procedure AfterConstruction; override;

  public
    procedure Open; override;
  end;

  {$ENDREGION Класс TCP - соединения сервера}

  {$REGION Класс последовательного порта, реализующий интерфейс IRtuConnection}
  { TRtuConnection }

  TRtuConnection = class(TBase, IRtuConnection)
  private
    fHandle: THandle;

    fBaudRate: dword;
    fParity:   Byte;
    fByteSize: Byte;
    fStopBits: Integer;
    fThreeAndHalf: DWord;

    fPortName: array[0 .. 1023] of Char;

  protected
    function GetHandle: THandle;

    function GetPortName: PChar;

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

  public
    constructor Create(const aRtuStruct: PRtuStruct); reintroduce;
    destructor Destroy; override;

  public
    procedure Open;
    procedure Close;

    property BaudRate: dword read GetBaudRate write SetBaudRate;
    property ByteSize: Byte read GetByteSize write SetByteSize;
    property Parity: Byte read GetParity write SetParity;
    property PortName: PChar read GetPortName;
    property StopBits: Byte read GetStopBits write SetStopBits;
    property ThreeAndHalf: Byte read GetThreeAndHalf write SetThreeAndHalf;
    property Handle: THandle read GetHandle;

  end;

{$ENDREGION Класс последовательного порта, реализующий интерфейс IRtuConnection}

implementation

const
  ERROR_CREATE_SOCKET = 1;
  ERROR_OPEN_SOCKET = 2;
  NOT_USED = 3;
  ERROR_SOCK_ADDR = 4;
  ERROR_BIND_SOCKET = 5;
  ERROR_LISTEN_SOCKET = 6;

resourcestring
  sERROR_CREATE_SOCKET = 'Ошибка создание сокета';
  sERROR_OPEN_SOCKET = 'Ошибка открытия сокета';
  sNOT_USED = 'Нельзя использовать';
  sERROR_SOCK_ADDR = 'Неверый адрес порта';
  sERROR_BIND_SOCKET = 'Ошибка привязки сокета';
  sERROR_LISTEN_SOCKET = 'Ошибка установки прослушивания порта';

{$REGION ПОЛУЧИТЬ IP, ПОРТ по сокету}
function GetLocalSocketPort(s: TSocket): Integer;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := sizeof(Addr);
  getsockname(s, {%H-}Addr, Size);
  Result := ntohs(Addr.sin_port);
end;

function GetLocalSocketAddress(s: TSocket): String;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := sizeof(Addr);
  getsockname(s, {%H-}Addr, Size);
  Result := inet_ntoa(Addr.sin_addr);
end;

function GetRemoteSocketPort(s: TSocket): Integer;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := sizeof(Addr);
  getpeername(s, {%H-}Addr, Size);
  Result := ntohs(Addr.sin_port);
end;

function GetRemoteSocketAddress(s: TSocket): String;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := sizeof(Addr);
  getpeername(s, {%H-}Addr, Size);
  Result := inet_ntoa(Addr.sin_addr);
end;

{$ENDREGION ПОЛУЧИТЬ IP, ПОРТ по сокету}

{$REGION Абстрактный класс TCP - соединения, реализующий ITcpConnection}
{ TTcpConnection }

destructor TTcpConnection.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TTcpConnection.GetIp: String;
begin
  fIp := String(fIpChar);
  Result := fIp;
end;

function TTcpConnection.GetPort: Word;
begin
  Result := fPort;
end;

constructor TTcpConnection.Create;
begin
  inherited Create;
  fSocket := INVALID_SOCKET;
end;

procedure TTcpConnection.Close;
begin
  CloseSocket(fSocket);
  fSocket := INVALID_SOCKET;
end;

function TTcpConnection.GetSocket: TSocket;
begin
  Result := fSocket;
end;

{$ENDREGION Абстрактный класс TCP - соединения, реализующий ITcpConnection}

{$REGION Класс TCP - соединения клиента}
{ TCnTcpConnection }

procedure TCnTcpConnection.SetIp(const aIp: String);
begin
  if not SameText(fIp, aIp) then
    StrPCopy(fIpChar, aIp);
end;

procedure TCnTcpConnection.SetPort(const aPort: Word);
begin
  if fPort <> aPort then
    fPort := aPort;
end;

constructor TCnTcpConnection.Create(const aIp: String; const aPort: Word);
begin
  inherited Create;
  StrPCopy(fIpChar, aIp);
  fPort := aPort;
end;

procedure TCnTcpConnection.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors.Add(ERROR_CREATE_SOCKET, sERROR_CREATE_SOCKET);
  fErrors.Add(ERROR_OPEN_SOCKET, sERROR_OPEN_SOCKET);
  fErrors.Add(NOT_USED, sNOT_USED);
end;

procedure TCnTcpConnection.Open;
var
  SockAddrIn: TSockAddrIn;
begin

  // Create Socket
  fSocket := WSASocket(AF_INET, SOCK_STREAM, 0, nil, 0, 0);
  if WSAGetLastError <> 0 then
//  if fSocket = INVALID_SOCKET then
  begin
    fLastError := ERROR_CREATE_SOCKET;
    Exit;
  end;

  // SockAddrIn
  ZeroMemory(@SockAddrIn, SizeOf(SockAddrIn));
  SockAddrIn.sin_family := AF_INET;
  SockAddrIn.sin_port   := htons(fPort);
  SockAddrIn.sin_addr.s_addr := inet_addr(fIpChar);

  // Open Socket
  if (WSAConnect(fSocket, @SockAddrIn, SizeOf(TSockAddr), nil, nil, nil, nil) =
    SOCKET_ERROR) and (WSAGetLastError <> WSAEWOULDBLOCK) then
  begin
    Close;
    fLastError := ERROR_OPEN_SOCKET;
    Exit;
  end;
end;

{$ENDREGION Класс TCP - соединения клиента}

{$REGION Класс TCP - соединения серверного клиента}
{ TAcTcpConnection }

constructor TAcTcpConnection.Create(const aSocket: TSocket);
begin
  inherited Create;
  fSocket := aSocket;
end;

procedure TAcTcpConnection.AfterConstruction;
begin
  inherited AfterConstruction;
  fPort := GetRemoteSocketPort(fSocket);
  StrPCopy(fIpChar, GetRemoteSocketAddress(fSocket));
end;

procedure TAcTcpConnection.SetIp(const aIp: String);
begin
  fLastError := NOT_USED;
end;

procedure TAcTcpConnection.SetPort(const aPort: Word);
begin
  fLastError := NOT_USED;
end;

procedure TAcTcpConnection.Open;
begin
  { TODO : Stub }
end;

{$ENDREGION Класс TCP - соединения серверного клиента}

{$REGION Класс TCP - соединения сервера}
{ TSvTcpConnection }
constructor TSvTcpConnection.Create(const aPort: Word);
begin
  inherited Create;
  fSocket := INVALID_SOCKET;
  fPort   := aPort;
end;

procedure TSvTcpConnection.AfterConstruction;
begin
  inherited AfterConstruction;

  fErrors.Add(ERROR_SOCK_ADDR, sERROR_SOCK_ADDR);
  fErrors.Add(ERROR_BIND_SOCKET, sERROR_BIND_SOCKET);
  fErrors.Add(ERROR_LISTEN_SOCKET, sERROR_LISTEN_SOCKET);
end;

procedure TSvTcpConnection.SetIp(const aIp: String);
begin
  fLastError := NOT_USED;
end;

procedure TSvTcpConnection.SetPort(const aPort: Word);
begin
  if fPort <> aPort then
    fPort := aPort;
end;

procedure TSvTcpConnection.Open;
var
  SockAddr: TSockAddr;   // Адрес привязки
begin
  // Формируем адрес для привязки.
  FillChar({%H-}SockAddr.sin_zero, SizeOf(SockAddr.sin_zero), 0);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_addr.S_addr := INADDR_ANY;
  SockAddr.sin_port := htons(fPort);
  if SockAddr.sin_port = 0 then
  begin
    SetLastError(ERROR_SOCK_ADDR);
    Exit;
  end;

  // Создание сокета
  fSocket := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if fSocket = INVALID_SOCKET then
  begin
    SetLastError(ERROR_OPEN_SOCKET);
    Exit;
  end;

  // Привязка сокета к адресу
  if Bind(fSocket, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
  begin
    SetLastError(ERROR_BIND_SOCKET);
    Close;
    Exit;
  end;

  // Перевод сокета в режим прослушивания
  if Listen(fSocket, SOMAXCONN) = SOCKET_ERROR then
  begin
    SetLastError(ERROR_LISTEN_SOCKET);
    Close;
    Exit;
  end;
end;

{$ENDREGION Класс TCP - соединения сервера}

{$REGION Класс последовательного порта, реализующий интерфейс IRtuConnection}
{ TRtuConnection }

constructor TRtuConnection.Create(const aRtuStruct: PRtuStruct);
var
  P: PChar;
begin
  inherited Create;

  fHandle := INVALID_HANDLE_VALUE;


  P := fPortName;
  StrPCopy(P, '\\.\' + aRtuStruct^.PortName);

  fBaudRate := aRtuStruct^.BaudRate;
  fByteSize := aRtuStruct^.ByteSize;
  fParity   := aRtuStruct^.Parity;
  fStopBits := aRtuStruct^.StopBits;
  fThreeAndHalf := aRtuStruct^.ThreeAndHalf;
end;

destructor TRtuConnection.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TRtuConnection.Open;
var
  DCB:     TDCB;
  //EvtMask: DWord;
  Flag:    DWord;
  CommTimeouts: TCommTimeouts;
begin
  // Создаем файл
  fHandle := CreateFile(fPortName, GENERIC_READ or
    GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
  if Windows.GetLastError <> 0 then
    Exit;

  // Производим настройку

  // DCB-структуры
  if not GetCommState(fHandle, {%H-}DCB) then
  begin
    Close;
    Exit;
  end;

  DCB.BaudRate := fBaudRate;
  DCB.ByteSize := fByteSize;
  DCB.Parity   := fParity;
  DCB.StopBits := fStopBits;

  if not SetCommState(fHandle, DCB) then
  begin
    Close;
    Exit;
  end;
  sleep(60);

  // Timeouts
  ZeroMemory(@CommTimeouts, SizeOf(CommTimeouts));
  CommTimeouts.ReadIntervalTimeout := fThreeAndHalf;
  if not SetCommTimeouts(FHandle, CommTimeouts) then
  begin
    Close;
    Exit;
  end;

  // Buffers
  if not SetupComm(fHandle, BUFFER_SIZE, BUFFER_SIZE) then
  begin
    Close;
    Exit;
  end;

  Flag := 0;
  Flag := Flag or PURGE_TXCLEAR or PURGE_RXCLEAR;
  if not PurgeComm(FHandle, Flag) then
  begin
    Close;
    Exit;
  end;
end;

procedure TRtuConnection.Close;
begin
  CloseHandle(fHandle);
  fHandle := INVALID_HANDLE_VALUE;
end;

function TRtuConnection.GetHandle: THandle;
begin
  Result := fHandle;
end;

function TRtuConnection.GetPortName: PChar;
begin
  Result := fPortName;
end;

function TRtuConnection.GetBaudRate: dword;
begin
  Result := fBaudRate;
end;

procedure TRtuConnection.SetBaudRate(const aBaudRate: dword);
begin
  if fBaudRate <> aBaudRate then
    fBaudRate := aBaudRate;
end;

function TRtuConnection.GetParity: Byte;
begin
  Result := fParity;
end;

procedure TRtuConnection.SetParity(const aParity: Byte);
begin
  if fParity <> aParity then
    fParity := aParity;
end;

function TRtuConnection.GetByteSize: Byte;
begin
  Result := fByteSize;
end;

procedure TRtuConnection.SetByteSize(const aByteSize: Byte);
begin
  if fByteSize <> aByteSize then
    fByteSize := aByteSize;
end;

function TRtuConnection.GetStopBits: Byte;
begin
  Result := fStopBits;
end;

procedure TRtuConnection.SetStopBits(const aStopBits: Byte);
begin
  if fStopBits <> aStopBits then
    fStopBits := aStopBits;
end;

function TRtuConnection.GetThreeAndHalf: Byte;
begin
  Result := fThreeAndHalf;
end;

procedure TRtuConnection.SetThreeAndHalf(const aThreeAndHalf: Byte);
begin
  if fThreeAndHalf <> aThreeAndHalf then
    fThreeAndHalf := aThreeAndHalf;

end;

{$ENDREGION Класс последовательного порта, реализующий интерфейс IRtuConnection}
end.
