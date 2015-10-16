unit uModbus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows,
  fgl,
  WinSock2;

const
  // Сообщение о новом клиенте
  WM_NEW_CLIENT = WM_USER + 10;


type

  //////////////////////////////////////////////////////////////////////////////
  // БАЗОВЫЕ ИНТЕРФЕЙСЫ

  // Базовый интерфейс библиотеки
  IBase = interface
    ['{35A79174-0FF1-42D9-8F0B-22FD490C7E7A}']
    // Возвращает код ошибки, одновременно сбрасывая его в памяти
    function GetLastError: dword;
    // Возвращает описание ошибки, в т.ч. системной
    function GetErrorDesc(const aError: dword): string;
  end;

  // Интерфейс потокобезопасного класса
  generic IThreadSafeWrapper<T> = interface(IBase)
    ['{5CE5A3ED-5F97-4D4E-A320-BA2FEEAC7B7E}']
    function Lock: T;
    procedure Unlock;
  end;

  // Приоритет объекта в очереди
  TPriority = (prLow, prHigh);

  // Базовый интерфейс элемента для очереди
  IPriorityItem = interface(IBase)
    ['{F721F50F-5306-4EC6-81EB-42242D0BEC6C}']
    function GetPriority: TPriority;
    procedure SetPriority(const aPriority: TPriority);
    property Priority: TPriority read GetPriority write SetPriority;
  end;

//////////////////////////////////////////////////////////////////////////////
// Буфер обмена
const
  BUFFER_SIZE = 1024;

type
  TBuffer = array[0 .. BUFFER_SIZE - 1] of byte;
  PBuffer = ^TBuffer;

  IBufferSpec = specialize IThreadSafeWrapper<TBuffer>;

  IClipboard = interface(IBufferSpec)
    ['{66675BDC-DFD5-42FC-AF8F-3263FBB2E8C2}']

    function GetCount: longint;
    procedure SetCount(const aCount: longint);

    function GetBuffer: PBuffer;

    property Count: longint read GetCount write SetCount;
    property Buffer: PBuffer read GetBuffer;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // MODBUS пакеты

  // PDU = Function Code + Data
  TPdu = TBuffer;
  PPdu = ^TPdu;


  // SlaveId        - адрес устройства
  // Request PDU    - PDU запроса
  // Response PDU   - PDU ответа
  // Timeout        - таймаут ответа
  // Responded   - флаг ответа
  IFrame = interface(IPriorityItem)
    ['{ED186020-2C0C-4390-8385-7BD3F3CCD054}']

    function GetSlaveId: byte;
    procedure SetSlaveId(const aSlaveId: byte);

    function GetRequestCount: longint;
    procedure SetRequestCount(const aCount: longint);

    function GetResponseCount: longint;
    procedure SetResponseCount(const aCount: longint);

    function GetRequestPdu: PPdu;
    function GetResponsePdu: PPdu;

    function GetTimeout: dword;
    procedure SetTimeout(const aTimeout: dword);

    function GetResponded: boolean;
    procedure SetResponded(const aResponded: boolean);

    function ToString: string;

    property SlaveId: byte read GetSlaveId write SetSlaveId;

    property RequestCount: longint read GetRequestCount write SetRequestCount;
    property ResponseCount: longint read GetResponseCount write SetResponseCount;

    property RequestPdu: PPdu read GetRequestPdu;
    property ResponsePdu: PPdu read GetResponsePdu;

    property Timeout: dword read GetTimeout write SetTimeout;

    property Responded: boolean read GetResponded write SetResponded;

  end;


  //////////////////////////////////////////////////////////////////////////////
  // ИНТЕРФЕЙСЫ MODBUS СТЕКА

  // Типы Modbus Controller
  TTypeController = (mbRtu, mbAcTcp, mbCnTcp);

  // Компоненты Modbus Controller
  IController = interface;
  ITransaction = interface;
  ICommunication = interface;
  ITcpConnection = interface;
  IRtuConnection = interface;

  IController = interface(IBase)
    ['{B538C91A-DEAD-403E-8A08-B10869545634}']
    function IsOpen: boolean;
    procedure Open;
    procedure Close;
    function InQueue(const aFrame: IFrame): boolean;

    function GetTransaction: ITransaction;
    procedure SetTransaction(const aTransaction: ITransaction);

    function GetTypeController: TTypeController;
    procedure SetTypeController(const aTypeController: TTypeController);

    function ToString: string;


    property Transaction: ITransaction read GetTransaction write SetTransaction;
    property TypeController: TTypeController
      read GetTypeController write SetTypeController;
  end;

  ITransaction = interface(IBase)
    ['{FB1CD02E-D26D-4734-B5FC-D1B5212C1DE4}']

    procedure Transact(const aFrame: IFrame);

    function GetCommunication: ICommunication;
    procedure SetCommunication(const aCommunication: ICommunication);
    property Communication: ICommunication read GetCommunication write SetCommunication;
  end;

  ICommunication = interface(IClipboard)
    ['{12C43F93-1EBE-45C5-8A99-90F2F1D1F549}']

    procedure Connect;
    procedure Disconnect;
    function IsConnected: boolean;

    function GetSentEvent: THandle;
    function GetReadyReadEvent: THandle;

    // Сигнализировать поток о необходимости отправки данных
    procedure NotifyToSend;
    // Событие ожидания отправки данных на устройство
    property SentEvent: THandle read GetSentEvent;
    // Событие ожидания записи во внутренний буфер обмена
    property ReadyReadEvent: THandle read GetReadyReadEvent;
  end;

  IRtuCommunication = interface(ICommunication)
    ['{68450535-CC74-4287-93A8-FC443C16143B}']

    function GetRtuConnection: IRtuConnection;
    procedure SetRtuConnection(const aRtuConnection: IRtuConnection);

    property RtuConnection: IRtuConnection read GetRtuConnection write SetRtuConnection;
  end;

  ITcpCommunication = interface(ICommunication)
    ['{68450535-CC74-4287-93A8-FC443C16143B}']

    function GetTcpConnection: ITcpConnection;
    procedure SetTcpConnection(const aTcpConnection: ITcpConnection);

    property TcpConnection: ITcpConnection read GetTcpConnection write SetTcpConnection;
  end;

  ITcpConnection = interface(IBase)
    ['{CEFC4411-F835-4193-B426-55D1FBD26C81}']
    // Процедуры открытия и закрытия сокета
    procedure Open;
    procedure Close;

    // Ip
    function GetIp: string;
    procedure SetIp(const aIp: string);

    // Порт
    function GetPort: word;
    procedure SetPort(const aPort: word);

    // Сокет
    function GetSocket: TSocket;

    property Ip: string read GetIp write SetIp;
    property Port: word read GetPort write SetPort;
    property Sock: TSocket read GetSocket;
  end;

  TRtuStruct = record
    PortName: PChar;
    // CBR_1200 = 1200;
    // CBR_2400 = 2400;
    // CBR_4800 = 4800;
    // CBR_9600 = 9600;
    // CBR_14400 = 14400;
    // CBR_19200 = 19200;
    // CBR_38400 = 38400;
    // CBR_56000 = 56000;
    // CBR_57600 = 57600;
    // CBR_115200 = 115200;
    // CBR_128000 = 128000;
    // CBR_256000 = 256000;
    BaudRate: dword;
    // NOPARITY = 0;
    // ODDPARITY = 1;
    // EVENPARITY = 2;
    // MARKPARITY = 3;
    // SPACEPARITY = 4;
    Parity: byte;
    // 4..8
    ByteSize: byte;
    // ONESTOPBIT = 0;
    // ONE5STOPBITS = 1;
    // TWOSTOPBITS = 2;
    StopBits: byte;
    // 20 .. 50
    ThreeAndHalf: byte;
  end;
  PRtuStruct = ^TRtuStruct;

  IRtuConnection = interface(IBase)
    ['{BE1FE747-D10D-48FF-9CB2-893BD2F283D1}']

    procedure Open;
    procedure Close;

    function GetHandle: THandle;

    function GetPortName: PChar;

    function GetBaudRate: dword;
    procedure SetBaudRate(const aBaudRate: dword);

    function GetParity: byte;
    procedure SetParity(const aParity: byte);

    function GetByteSize: byte;
    procedure SetByteSize(const aByteSize: byte);

    function GetStopBits: byte;
    procedure SetStopBits(const aStopBits: byte);

    function GetThreeAndHalf: byte;
    procedure SetThreeAndHalf(const aThreeAndHalf: byte);

    property BaudRate: dword read GetBaudRate write SetBaudRate;
    property ByteSize: byte read GetByteSize write SetByteSize;
    property Parity: byte read GetParity write SetParity;
    property PortName: PChar read GetPortName;
    property StopBits: byte read GetStopBits write SetStopBits;
    property ThreeAndHalf: byte read GetThreeAndHalf write SetThreeAndHalf;
    property Handle: THandle read GetHandle;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // СТРОИТЕЛИ MODBUS ПРОТОКОЛА

  // Прототипы строителей
  IBuilder = interface;
  IRtuBuilder = interface;
  ICnTcpBuilder = interface;
  IAcTcpBuilder = interface;

  // Карта отображения строителей
  TBuildersMap = specialize TFpgMap<TTypeController, IBuilder>;

  // Интерфейс коллекции строителей Modbus controller
  IBuilderSpec = specialize IThreadSafeWrapper<TBuildersMap>;

  { IStackBuilder }

  IStackBuilder = interface(IBuilderSpec)
    ['{60DC5F85-DCB9-4975-9191-C21184D1F7CD}']
    function GetRtuBuilder: IRtuBuilder;
    function GetCnTcpBuilder: ICnTcpBuilder;
    function GetAcTcpBuilder: IAcTcpBuilder;

    property RtuBuilder: IRtuBuilder read GetRtuBuilder;
    property CnTcpBuilder: ICnTcpBuilder read GetCnTcpBuilder;
    property AcTcpBuilder: IAcTcpBuilder read GetAcTcpBuilder;
  end;

  // Общий интерфейс строителя Modbus Controller
  IBuilder = interface(IBase)
    ['{6C180F30-56E0-4E9C-BBD1-0745421B59CB}']
    // Этапы построения контроллера
    procedure BuildController;
    procedure BuildTransaction;
    procedure BuildCommunication;
    procedure BuildConnection;
  end;

  IRtuBuilder = interface(IBuilder)
    ['{1ACBBF0A-21AE-40F4-A9C1-B61968D38522}']
    function GetController(const aPortName: PChar; const aBaudRate: dword;
      const aParity: byte; const aStopBits: byte; const aByteSize: byte;
      const aThreeAndHalf: byte): IController;
  end;

  ICnTcpBuilder = interface(IBuilder)
    ['{1ACBBF0A-21AE-40F4-A9C1-B61968D38522}']
    function GetController(const aIp: string; const aPort: word): IController;
  end;

  IAcTcpBuilder = interface(IBuilder)
    ['{1ACBBF0A-21AE-40F4-A9C1-B61968D38522}']
    function GetController(const aSocket: TSocket): IController;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // СЕРВЕР
  IControllerPool = interface;
  IServer = interface(IBase)
    ['{DB3BEF63-3F07-406E-B0E4-B3B7D4DBC6FD}']
    function Start: boolean;
    procedure Stop;

    function IsActive: boolean;

    function GetTcpConnection: ITcpConnection;
    procedure SetTcpConnection(const aTcpConnection: ITcpConnection);

    procedure SetAcTcpBuilder(const aAcTcpBuilder: IAcTcpBuilder);
    procedure SetControllerPool(const aControllerPool: IControllerPool);

    function GetHandle: THandle;
    procedure SetHandle(const aHandle: THandle);

    property TcpConnection: ITcpConnection read GetTcpConnection write SetTcpConnection;
    property AcTcpBuilder: IAcTcpBuilder write SetAcTcpBuilder;
    property ControllerPoll: IControllerPool write SetControllerPool;
    property Handle: THandle read GetHandle write SetHandle;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // ПУЛ КОНТРОЛЛЕРОВ

  TTypeListAction = (laAdded, laRemoved);
  TChangePoolEvent =
    procedure(const aController: IController;
    const aListAction: TTypeListAction) of object;

  TControllers = specialize TFpgList<IController>;

  IControllerPool = interface(IBase)
    ['{2979B60E-70D8-457F-AF14-575E550E9C26}']
    procedure Add(aController: IController);
    procedure Remove(aController: IController);
    function ReplaceConnection(const aClientSocket: TSocket): boolean;
    procedure SetOnChangePool(const aOnChangePool: TChangePoolEvent);
  end;

//////////////////////////////////////////////////////////////////////////////
// ПРОТОТИПЫ ИМПОРТИРУЕМЫХ ФУНКЦИЙ ИЗ MODBUS.DLL

function GetControllerPool: IControllerPool; external 'modbus.dll';

function GetServer(const aPort: word; const aHandle: THandle = 0): IServer; external 'modbus.dll';

function GetRtuBuilder: IRtuBuilder; external 'modbus.dll';

function GetTcpBuilder: ICnTcpBuilder; external 'modbus.dll';

function ReadInput(const aSlaveId: byte; const aStart, aRegCount: word;
  const aTimeout: dword): IFrame; external 'modbus.dll';

function ReadHolding(const aSlaveId: byte; const aStart, aRegCount: word;
  const aTimeout: dword): IFrame; external 'modbus.dll';

function WriteMultiple(const aSlaveId: byte; const aStart, aRegCount: word;
  const aValue, aTimeout: dword): IFrame; external 'modbus.dll';

function RunBootloader(const aSlaveId: Byte; const aTimeout: dword): IFrame;
  external 'modbus.dll';

function RunApplication(const aSlaveId: Byte; const aTimeout: dword): IFrame;
  external 'modbus.dll';
function ResetApplication(const aSlaveId: Byte; const aTimeout: dword): IFrame;  external 'modbus.dll';
function ReadSerial(const aSlaveId: Byte; const aTimeout: dword): IFrame;  external 'modbus.dll';


function WritePage(const aSlaveId: byte;
  const aCurrentPage, aSize: word; const aBuffer: PBuffer;
  const aTimeout: dword): IFrame; external 'modbus.dll';
function WriteStruct(const aSlaveId: byte; const aPdu: TBuffer;
  const aCount: word; const aTimeout: dword): IFrame; external 'modbus.dll';

procedure CalcCrc16(const aBuffer: PBuffer; const aCount: Longint;
      var aCrc16: Word); external 'modbus.dll';

implementation

end.









