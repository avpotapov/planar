unit uBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows,
  fgl,
  uModbus;

type

  { TBase }
  {
    Базовый класс библиотеки
    Содержит методы работы с кодами ошибок
    Все классы должны быть унаследованы от него
  }

  TBase = class(TInterfacedObject)
    type
    TErrors = specialize TFpgMap<Integer, String>;

  protected
    fLastError: dword;
    fErrors:    TErrors;

  public
    constructor Create;
    destructor Destroy; override;

    // Установить ошибку
    procedure SetLastError(const aError: dword);

    // Возвращает код ошибки, одновременно сбрасывая его в памяти
    function GetLastError: dword;
    // Возвращает описание ошибки, в т.ч. системной
    function GetErrorDesc(const aError: dword): String;
  end;

  { TPriorityItem }

 {

  Базовый класс-элемент кучи

 }
  TPriorityItem = class(TBase, IPriorityItem)
  private
    fPriority: TPriority;

  protected
    function GetPriority: TPriority;
    procedure SetPriority(const aPriority: TPriority);

  public
    constructor Create;
  public
    property Priority: TPriority read GetPriority write SetPriority;
  end;

  { THeap }

 {

  Куча для реализации очереди с приоритетами

  LeftChild = Root * 2 + 1
  RightChild = LeftChild + 1

  Parent = (New - 1) div 2

 }
  THeap = class(TBase)
  private
    fItems:    array of IPriorityItem;
    fSize:     Word;
    fCapacity: Word;

  private
    procedure Swap(aIndex1, aIndex2: Integer);

  protected
    procedure ReBuild(const aParent: Word); virtual;

  public
    constructor Create(const aCapacity: Word); reintroduce;
    procedure AfterConstruction; override;
    destructor Destroy; override;

  public
    function IsEmpty: Boolean;
    procedure Delete(out aItem: IPriorityItem);
    procedure Insert(const aItem: IPriorityItem);
  end;

  { TThreadSafeWrapper }
     {
       Потокобезопасный доступ к типу данных T (простой тип данных, массив,
       класс) через критическую секцию
     }
  generic TThreadSafeWrapper<T> = class(TBase)
  private
    fLock: TRtlCriticalSection;

  protected
    fT: T;

  public
    constructor Create;
    destructor Destroy; override;

    function Lock: T;
    procedure Unlock;
  end;



  { TThreadSafePriorityQueue }
  TThreadSafePriorityQueueSpec = specialize TThreadSafeWrapper<THeap>;

  TThreadSafePriorityQueue = class(TThreadSafePriorityQueueSpec)
  protected
    fPushEvent: THandle;

  public
    constructor Create(const aCapacity: Word);
      reintroduce; virtual;
    destructor Destroy; override;

  public
    function Push(const aItem: IPriorityItem): Boolean;
    function Pop(out aItem: IPriorityItem): Boolean;
  end;

  { TClipboard  - буфер обмена}

  TBufferSpec = specialize TThreadSafeWrapper<TBuffer>;

  TClipboard = class(TBufferSpec)
  private
    // Количество записанных в буфер обмена байт
    fCount: Longint;
  protected
    function GetCount: Longint;
    procedure SetCount(const aCount: Longint);

    function GetBuffer: PBuffer;
  public
    property Count: Longint read GetCount write SetCount;
    property Buffer: PBuffer read GetBuffer;
  end;

  { TCrc16 }

  TCrc16 = class
  const
    CrcTable: array [Byte] of Word = {$REGION CRC16 TABLE}

     ($0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0,
      $0780, $C741, $0500, $C5C1, $C481, $0440, $CC01, $0CC0, $0D80, $CD41,
      $0F00, $CFC1, $CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0,
      $0880, $C841, $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
      $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41, $1400, $D4C1,
      $D581, $1540, $D701, $17C0, $1680, $D641, $D201, $12C0, $1380, $D341,
      $1100, $D1C1, $D081, $1040, $F001, $30C0, $3180, $F141, $3300, $F3C1,
      $F281, $3240, $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
      $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41, $FA01, $3AC0,
      $3B80, $FB41, $3900, $F9C1, $F881, $3840, $2800, $E8C1, $E981, $2940,
      $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1,
      $EC81, $2C40, $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
      $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, $A001, $60C0,
      $6180, $A141, $6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740,
      $A501, $65C0, $6480, $A441, $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0,
      $6E80, $AE41, $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
      $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41, $BE01, $7EC0,
      $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40, $B401, $74C0, $7580, $B541,
      $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381, $7340, $B101, $71C0,
      $7080, $B041, $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
      $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, $9C01, $5CC0,
      $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40,
      $9901, $59C0, $5880, $9841, $8801, $48C0, $4980, $8941, $4B00, $8BC1,
      $8A81, $4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
      $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0,
      $4380, $8341, $4100, $81C1, $8081, $4040);
     {$ENDREGION CRC16 TABLE}

  public
    class procedure CalcCrc16(const aBuffer: PBuffer; const aCount: Longint;
      var aCrc16: Word);
  end;


implementation

{ TThreadSafePriorityQueue }

constructor TThreadSafePriorityQueue.Create(const aCapacity: Word);
begin
  inherited Create;
  fT := THeap.Create(aCapacity);
  fPushEvent := CreateEvent(nil, False, False, '');
end;

destructor TThreadSafePriorityQueue.Destroy;
begin
  CloseHandle(fPushEvent);
  inherited Destroy;
  FreeAndNil(fT);
end;

function TThreadSafePriorityQueue.Push(const aItem: IPriorityItem): Boolean;
var
  Heap: THeap;
begin
  Result := False;
  Heap   := Lock;
  try

    Heap.Insert(aItem);
    if Heap.GetLastError = 0 then
    begin
      // Сигнализировать поток о помещении в очередь очередного элемента
      SetEvent(fPushEvent);
      Result := True;
    end;

  finally
    Unlock;
  end;
end;

function TThreadSafePriorityQueue.Pop(out aItem: IPriorityItem): Boolean;
var
  Heap: THeap;
begin
  Result := False;
  Heap   := Lock;
  try

    Heap.Delete(aItem);
    if Heap.GetLastError = 0 then
      Result := True;

  finally
    Unlock;
  end;

end;


{ TBase }

constructor TBase.Create;
begin
  inherited Create;
  fErrors := TErrors.Create;
end;

destructor TBase.Destroy;
begin
  FreeAndNil(fErrors);
  inherited Destroy;
end;

procedure TBase.SetLastError(const aError: dword);
begin
  fLastError := aError;
end;

function TBase.GetLastError: dword;
begin
  Result     := fLastError;
  if fLastError = 0 then
    Result := GetLastError;
  fLastError := 0;
end;

function TBase.GetErrorDesc(const aError: dword): String;
begin
  if aError <> 0 then
    Result := fErrors.KeyData[aError]
  else
    Result := SysErrorMessage(Windows.GetLastError);
end;

{$REGION HEAP}

{ TPriorityItem }
constructor TPriorityItem.Create;
begin
  inherited Create;
  fPriority := TPriority.prLow;
end;

function TPriorityItem.GetPriority: TPriority;
begin
  Result := fPriority;
end;

procedure TPriorityItem.SetPriority(const aPriority: TPriority);
begin
  fPriority := aPriority;
end;

const
  ERROR_HEAP_FULL  = 1;
  ERROR_HEAP_EMPTY = 2;

resourcestring
  sERROR_HEAP_FULL  = 'Куча заполнена';
  sERROR_HEAP_EMPTY = 'Куча пуста';

{ THeap }

constructor THeap.Create(const aCapacity: Word);
begin
  inherited Create;
  fSize     := 0;
  // Емкость кучи
  fCapacity := aCapacity;
  SetLength(fItems, aCapacity);
end;

procedure THeap.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors.Add(ERROR_HEAP_FULL, sERROR_HEAP_FULL);
  fErrors.Add(ERROR_HEAP_EMPTY, sERROR_HEAP_EMPTY);
end;

destructor THeap.Destroy;
begin
  fItems := nil;
  inherited Destroy;
end;

function THeap.IsEmpty: Boolean;
begin
  //Определяет,пуста ли куча.
  //Предусловие:нет.
  //Постусловие:если куча пуста,возвращает значение true;
  //в противном случае возвращает значение false
  Result := fSize = 0;
end;

procedure THeap.Insert(const aItem: IPriorityItem);
var
  Place, Parent: Integer;
begin
  // Вставляет новый элемент за последним узлом кучи
  // и перемещает его вверх по дереву на соответствующую позицию.
  // Куча считается полной, если она содержит максимальное кол-во элементов.

  // Проверка места в кучи
  if fSize >= fCapacity then
  begin
    fLastError := ERROR_HEAP_FULL;
    Exit;
  end;

  //Размещаем новый элемент в конце кучи
  fItems[fSize] := aItem;

  //Перемещаем новый элемент на подходящую позицию
  Place  := fSize;
  Parent := (Place - 1) div 2;
  while (Parent >= 0) and (fItems[Place].Priority > fItems[Parent].Priority) do
  begin
    //Меняем местами элементы
    Swap(Place, Parent);

    Place  := Parent;
    Parent := (Place - 1) div 2;
  end;
  Inc(fSize, 1);
end;

procedure THeap.Delete(out aItem: IPriorityItem);
begin
  // Проверка кучи
  if IsEmpty then
  begin
    fLastError := ERROR_HEAP_EMPTY;
    Exit;
  end;

  //Извлекает и удаляет элемент из корня кучи.
  //Данный элемент имеет наибольшее значение ключа.
  // ! Указатель перемещаем, если не использовать fItems[0] := nil,
  // ! происходит утечка памяти
  aItem     := fItems[0];
  fItems[0] := nil;


  // Меняет местами последний элемент кучи с корнем
  // и перемещает его вниз подереву,пока не будет обнаружена подходящая позиция.
  fItems[0] := fItems[fSize - 1];
  fItems[fSize - 1] := nil;

  Dec(fSize, 1);
  Rebuild(0);

end;

procedure THeap.ReBuild(const aParent: Word);
var
  Child, RightChild: Word;
begin
  // Если корень не является листом,
  // и ключ корня меньше ключей его дочерних узлов

  //Индекс левого дочернего узла корня
  Child := aParent * 2 + 1;

  // ... если он существует
  if (Child < fSize) then
  begin
    //Корень не является листом, поэтому имеет левый дочерний узел

    //Индекс правого дочернего узла
    RightChild := Child + 1;

    // ... если он существует
    // ... найти наибольший элемент между дочерними узлами
    if (RightChild < fSize) and (fItems[RightChild].Priority >
      fItems[Child].Priority) then
      // Индекс наибольшего дочернего узла корня
      Child := RightChild;

    // Если ключ корня меньше ключа его наибольшего узла
    if (fItems[aParent].Priority < fItems[Child].Priority) then
    begin
      // Меняем местами элементы
      Swap(Child, aParent);

      // Преобразовываем новое поддерево в кучу
      Rebuild(Child);
    end;
  end;
end;

procedure THeap.Swap(aIndex1, aIndex2: Integer);
var
  Temp: IPriorityItem;
begin
  Temp := fItems[aIndex1];
  fItems[aIndex1] := fItems[aIndex2];
  fItems[aIndex2] := Temp;
end;

{$ENDREGION HEAP}


{ TThreadSafeWrapper }

constructor TThreadSafeWrapper.Create;
begin
  inherited Create;
  InitCriticalSection(fLock);
end;

destructor TThreadSafeWrapper.Destroy;
begin
  Lock;
  try
    inherited Destroy;
  finally
    Unlock;
    DoneCriticalSection(fLock);
  end;
end;

function TThreadSafeWrapper.Lock: T;
begin
  Result := fT;
  System.EnterCriticalSection(fLock);
end;

procedure TThreadSafeWrapper.Unlock;
begin
  System.LeaveCriticalSection(fLock);
end;

{ TClipboard }

function TClipboard.GetCount: Longint;
begin
  Result := Windows.InterlockedCompareExchange(fCount, 0, 0);
end;

procedure TClipboard.SetCount(const aCount: Longint);
begin
  Windows.InterLockedExchange(fCount, aCount);
end;

function TClipboard.GetBuffer: PBuffer;
begin
  Result := @fT;
end;

{ TCrc16 }

class procedure TCrc16.CalcCrc16(const aBuffer: PBuffer; const aCount: Longint;
  var aCrc16: Word);
var
    Index: Byte;
    I: Integer;
begin
  for I := 0 to aCount - 1 do
  begin
    Index := aBuffer^[I] xor aCrc16;
    aCrc16 := aCrc16 shr 8 xor CRCTable[Index];
  end;

end;

end.
