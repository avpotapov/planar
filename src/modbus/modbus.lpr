(*
 * Библиотека  Modbus
 * Реализует взаимодействие с устройством по протоколу Modbus
 *
 * Фабрика запросов modbus создает ReadInput, ReadHolding, WriteMultiple и т.д.
 * Запрос помещается в очередь контроллера.
 * Результат выполнения можно узнать с помощью функции 'Responded'.
 * Строитель создает определенный стек modbus (TCP, RTU) и настраивает его
 * должным образом.
 *)


library modbus;

{$mode objfpc}{$H+}

uses
  ShareMem, Classes, WinSock2, Windows,
  ubase,              // Модуль с базовыми классами
  userver,            // Сервер
  ucontrollerpool,    // Пул контроллеров (используется для серверного сокета)
  ucontroller,        // Уровни стека - контроллер
  utransaction,       // Уровни стека - транзакция
  ucommunication,     // Уровни стека - коммуникация
  uconnection,        // Уровни стека - соединение
  ustackbuilder,      // Строитель стека
  uframe,             // Фреймы modbus запросов
  uframefactory,      // Фабрика фреймов
  ulogger,            // Логгирование ошибок транзакций
  umodbus;            // Экспортный интерфейсный модуль

{$R *.res}

var
  WSAData:      TWSAData;
  {%H-}DLLProc: Pointer;

  // Глобальные объектные переменные
  StackBuilder: IStackBuilder;
  ControllerPool: IControllerPool;


  // Определение экспортируемых функций и процедур
  function GetControllerPool: IControllerPool;  export;
  begin
    if ControllerPool = nil then
       ControllerPool := TControllerPool.Create;
    Result := ControllerPool;
  end;

  function GetStackBuilder: IStackBuilder; export;
  begin
    if StackBuilder = nil then
       StackBuilder := TStackBuilder.Create;
    Result := StackBuilder;
  end;

  function GetRtuBuilder: IRtuBuilder;
  begin
    Result := GetStackBuilder.RtuBuilder;
  end;

  function GetTcpBuilder: ICnTcpBuilder;
  begin
    Result := GetStackBuilder.CnTcpBuilder;
  end;

  function GetServer(const aPort: word; const aHandle: THandle = 0): IServer; export;
  begin
    Result := TServer.Create(GetStackBuilder.GetAcTcpBuilder, GetControllerPool);
    Result.TcpConnection := TSvTcpConnection.Create(aPort);
    Result.Handle := aHandle;
  end;

  function ReadInput(const aSlaveId: Byte; const aStart,
   aRegCount: Word; const aTimeout: dword): IFrame; export;
  begin
    Result := TFrameFactory.ReadInput(aSlaveId, aStart, aRegCount, aTimeout);
  end;

  function ReadHolding(const aSlaveId: Byte; const aStart,
   aRegCount: Word; const aTimeout: dword): IFrame; export;
  begin
    Result := TFrameFactory.ReadHolding(aSlaveId, aStart, aRegCount, aTimeout);
  end;

  function WriteMultiple(const aSlaveId: Byte; const aStart,
   aRegCount: Word; const aValue, aTimeout: dword): IFrame; export;
  begin
    Result := TFrameFactory.WriteMultiple(aSlaveId, aStart, aRegCount, aValue, aTimeout);
  end;

  function RunApplication(const aSlaveId: Byte; const aTimeout: dword): IFrame; export;
  begin
    Result := TFrameFactory.RunApplication(aSlaveId, aTimeout);
  end;

  function RunBootloader(const aSlaveId: Byte; const aTimeout: dword): IFrame; export;
  begin
    Result := TFrameFactory.RunBootloader(aSlaveId, aTimeout);
  end;

  function WritePage(const aSlaveId: byte;
    const aCurrentPage, aSize: word; const aBuffer: PBuffer;
    const aTimeout: dword): IFrame; export;
  begin
    Result := TFrameFactory.WritePage(aSlaveId, aCurrentPage, aSize, aBuffer,
      aTimeout);
  end;



  procedure CalcCrc16(const aBuffer: PBuffer; const aCount: Longint;
      var aCrc16: Word); export;
  begin
    TCrc16.CalcCrc16(aBuffer, aCount, aCrc16);
  end;



// Список экспортируемых функций  процедур
exports
  GetControllerPool,

  GetServer,

  GetRtuBuilder,
  GetTcpBuilder,

  ReadInput,
  ReadHolding,
  WriteMultiple,

  RunApplication,
  RunBootloader,
  WritePage,

  CalcCrc16;

// Инициализация библиотеки WinSock
procedure DLLEntryPoint(Reason: Word);
begin
  case Reason of
    DLL_PROCESS_ATTACH: WSAStartup($101, WSAData);
    DLL_PROCESS_DETACH: WSACleanup;
  end;
end;

begin
DLLProc := @DLLEntryPoint;
DLLEntryPoint(DLL_PROCESS_ATTACH);
end.

