unit uController;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}// Альтернатива -dDEBUG
{$C+}// Альтернатива Включить Assert
interface

uses
  Classes, SysUtils,
  Windows,
  uBase,
  uModbus;

type

  { TController }

  TController = class(TThreadSafePriorityQueue, IController)
  private
    fName: string;

    // Флаг открыта ли очередь
    fIsOpen: Integer;

    // Указатель на объект транзакции: Tcp или Rtu
    fTransaction: ITransaction;

    // Дескриптор потока
    fThreadId:   TThreadId;
    // Событие: прервать поток по требованию
    fBreakEvent: THandle;
    // Массив ожидаемых событий
    fEvents:     array[0..1] of THandle;

    //
    fTypeController: TTypeController;
  private
    function GetTransaction: ITransaction;
    procedure SetTransaction(const aTransaction: ITransaction);

    function GetTypeController: TTypeController;
    procedure SetTypeController(const aTypeController: TTypeController);

  public
    constructor Create(const aCapacity: Word); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function ToString: string; override;


  public
    function IsOpen: Boolean;
    procedure Open;
    procedure Close;
    function InQueue(const aFrame: IFrame): Boolean;

    property Transaction: ITransaction read GetTransaction write SetTransaction;
    property TypeController: TTypeController read GetTypeController write SetTypeController;

  end;

implementation


const
  ERROR_NOT_OPEN = 1;
  ERROR_READY_OPEN = 2;
  ERROR_PUSH   = 3;
  ERROR_THREAD = 4;

resourcestring
  sERROR_NOT_OPEN = 'Контроллер отключен';
  sERROR_READY_OPEN = 'Контроллер уже запущен';
  sERROR_PUSH   = 'Ошибка постановки запроса в очередь';
  sERROR_THREAD = 'Ошибка выполнения потока';

{ TController }

function DeQueue(aParameter: Pointer): Integer;
var
  Controller: TController;
  Item: IPriorityItem;
begin
  Result := 0;
  {$IFDEF DEBUG}
  Assert(aParameter <> nil);
  Assert(TObject(aParameter) is TController);
  {$ENDIF}
  Controller := TController(aParameter);
  while True do
    case WaitForMultipleObjects(2, @Controller.fEvents, False, INFINITE) of

      // Событие: Закрыть поток
      WAIT_OBJECT_0:
        Break;

      // Событие: Чтение из очереди
      WAIT_OBJECT_0 + 1:
        // Пока очередь не пуста
        while Controller.Pop(Item) do
          // Выполнение транзакции
           Controller.fTransaction.Transact(Item as IFrame);

      else
        // Ошибка функции ожидания
      begin
        Result := ERROR_THREAD;
        Break;
      end;
    end;
end;

function TController.GetTransaction: ITransaction;
begin
  Result := fTransaction;
end;

procedure TController.SetTransaction(const aTransaction: ITransaction);
begin
  fTransaction := aTransaction;
end;

function TController.GetTypeController: TTypeController;
begin
  Result := fTypeController;
end;

procedure TController.SetTypeController(const aTypeController: TTypeController);
begin
  if fTypeController <> aTypeController then
     fTypeController := aTypeController;
end;

constructor TController.Create(const aCapacity: Word);
begin
  inherited Create(aCapacity);
  fIsOpen     := 0;
  fBreakEvent := CreateEvent(nil, False, False, '');
end;

procedure TController.AfterConstruction;
begin
  inherited AfterConstruction;
  {$IFDEF DEBUG}
  Assert(fPushEvent <> 0);
  Assert(fBreakEvent <> 0);
  {$ENDIF}

  {$IFDEF DEBUG}
  Assert(fErrors <> nil);
  {$ENDIF}

  // Массив событий
  fEvents[0] := fBreakEvent;
  fEvents[1] := fPushEvent;

  // Карта ошибок
  fErrors.Add(ERROR_NOT_OPEN, sERROR_NOT_OPEN);
  fErrors.Add(ERROR_READY_OPEN, sERROR_READY_OPEN);
  fErrors.Add(ERROR_PUSH, sERROR_PUSH);
  fErrors.Add(ERROR_THREAD, sERROR_THREAD);

  // Запуск потока изъятия элемента из списка
  fThreadId := BeginThread(@DeQueue, Self);
end;

destructor TController.Destroy;
begin
  CloseHandle(fBreakEvent);
  inherited Destroy;
end;

procedure TController.BeforeDestruction;
const
  TIMEOUT_CLOSE = 5000;
begin
  // Закрыть очередь
  Close;

  // Закрытие потока
  if fThreadId <> 0 then
  begin
    SetEvent(fBreakEvent);
    WaitForThreadTerminate(fThreadId, TIMEOUT_CLOSE);
    GetExitCodeThread(fThreadId, fLastError);
    CloseThread(fThreadId);
    fThreadId := 0;
  end;

  inherited BeforeDestruction;
end;

function TController.ToString: string;
var
  S: string;
begin

  case TypeController of
  mbRtu:
    begin
      S := string((Transaction.Communication as IRtuCommunication).RtuConnection.PortName);
      fName := Copy(S, 5, Length(S));
    end;
  mbAcTcp:
    fName := Format ('%s (сервер)',
      [(Transaction.Communication as ITcpCommunication).TcpConnection.GetIp]);
  mbCnTcp:
    fName := Format ('%s: %d',
      [ITcpCommunication(Transaction.Communication as ITcpCommunication).TcpConnection.GetIp,
       ITcpCommunication(Transaction.Communication as ITcpCommunication).TcpConnection.Port]);
  end;

  Result := fName;

end;

procedure TController.Open;
begin
  {$IFDEF DEBUG}
  Assert(fTransaction <> nil);
  {$ENDIF}
  Windows.InterLockedExchange(fIsOpen, 1);
end;

procedure TController.Close;
begin
  Windows.InterLockedExchange(fIsOpen, 0);
  Sleep(100);
  Transaction.Communication.Disconnect;
end;

function TController.IsOpen: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fIsOpen, 0, 0) = 1;
end;

function TController.InQueue(const aFrame: IFrame): Boolean;
begin
  Result := False;
  {$IFDEF DEBUG}
  Assert(aFrame <> nil);
  {$ENDIF}

  // Контроллер закрыт
  if not IsOpen then
  begin
    fLastError := ERROR_NOT_OPEN;
    Exit;
  end;

  // Сброс события ожидания, если оно оказалось сигнальным в результате
  // неудачной предыдущей транзакции
//  aFrame.PrepareAsync;
  aFrame.Responded:= False;
  Result := Push(aFrame);
  if not Result then
    fLastError := ERROR_PUSH;
end;


end.
