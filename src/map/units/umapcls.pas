unit uMapCls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows,
  uMap, uStatus, uStatusSaxParser;

type

  { TBaseMap }

  TBaseMap = class(TInterfacedObject, IBaseMap)
  private
    Tables: TTables;
    fLock:  TRtlCriticalSection;
  public
    function Lock(const aTypeTable: TTypeTable = TTypeTable.ttInput): PTable;
    procedure UnLock;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMap }

  TMap = class(TBaseMap, IMap)
  private
    fStatus: TStatus;
    fValue:  string;

  public
    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    procedure WriteData(const aOffSet: word; const aPdu: PPdu);
    procedure WriteToTable(const aOffSet: word; const aRequestPdu: PPdu);

    function ReadIoData(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword = 1): string;

    function ReadIpAddress(const aTypeTable: TTypeTable; const aOffSet: word): string;

    function ReadUint8l(const aTypeTable: TTypeTable; const aOffSet: word): byte; overload;
    function ReadUint8l(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single; overload;

    function ReadUint8h(const aTypeTable: TTypeTable; const aOffSet: word): byte; overload;
    function ReadUint8h(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single; overload;

    function ReadUint16(const aTypeTable: TTypeTable; const aOffSet: word): word; overload;
    function ReadUint16(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single; overload;

    function ReadUint32(const aTypeTable: TTypeTable; const aOffSet: word): dword; overload;
    function ReadUint32(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single; overload;

    function ReadSint16(const aTypeTable: TTypeTable; const aOffSet: word): smallInt; overload;
    function ReadSint16(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;  overload;

    function ReadSint32(const aTypeTable: TTypeTable; const aOffSet: word): longint; overload;
    function ReadSint32(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;  overload;

    function ReadBitmap16(const aTypeTable: TTypeTable; const aOffSet: word): word;  overload;
    function ReadBitmap32(const aTypeTable: TTypeTable; const aOffSet: word): dword; overload;

    function ReadFloat(const aTypeTable: TTypeTable; const aOffSet: word): single;   overload;

    function ReadBool(const aTypeTable: TTypeTable; const aOffSet: word): boolean;  overload;

    procedure Clear;
  end;



implementation



{ TBaseMap }

function TBaseMap.Lock(const aTypeTable: TTypeTable): PTable;
begin
  System.EnterCriticalSection(fLock);
  Result := @Tables[aTypeTable];
end;

procedure TBaseMap.UnLock;
begin
  System.LeaveCriticalSection(fLock);
end;

constructor TBaseMap.Create;
var
  I: TTypeTable;
begin
  inherited Create;
  InitCriticalSection(fLock);
  for I := Low(TTypeTable) to High(TTypeTable) do
    ZeroMemory(@Tables[I], Length(Tables[I]));
end;

destructor TBaseMap.Destroy;
begin
  Lock;
  try
    inherited Destroy;
  finally
    Unlock;
    DoneCriticalSection(fLock);
  end;
end;


{ TMap }

constructor TMap.Create;
begin
  inherited Create;
  fStatus := TStatus.Create;
end;

procedure TMap.AfterConstruction;
begin
  inherited AfterConstruction;

  with TStatusFactory.Create(fStatus) do
    try
      Parse;
    finally
      Free;
    end;

end;

destructor TMap.Destroy;
begin
  FreeAndNil(fStatus);
  inherited Destroy;
end;

procedure TMap.WriteData(const aOffSet: word; const aPdu: PPdu);
var
  Table: PTable;
  TypeTable: TTypeTable;
  NRegs: byte;
  Wrd: word;

  TableOffset: word;
  PduOffset:   word;
begin

  // Тип таблицы
  case aPdu^[0] of
    3: TypeTable := TTypeTable.ttHolding;
    4: TypeTable := TTypeTable.ttInput;
    else
      Exit;
  end;

  // количество регистров для записи
  NRegs := aPdu^[1] shr 1;

  // Начальное смещение в PDU
  PduOffset := 2;

  // Начальное смещение в Table
  TableOffset := aOffSet;

  // Таблица с данными
  Table := Lock(TypeTable);
  try

    while NRegs > 0 do
    begin

      // Слово из PDU
      Wrd := PWord(@aPdu^[PduOffset])^;
      // Записать своппированное слово в Table
      Table^[TableOffset] := Swap(Wrd);

      // Счетчики
      Inc(PduOffset, 2);
      Inc(TableOffset, 1);
      Dec(NRegs, 1);
    end;

  finally
    Unlock;
  end;
end;

procedure TMap.WriteToTable(const aOffSet: word; const aRequestPdu: PPdu);
var
  Table: PTable;
  TypeTable: TTypeTable;
  NRegs: byte;
  Wrd: word;

  TableOffset: word;
  PduOffset:   word;
begin

  // Тип таблицы
  case aRequestPdu^[0] of
    3: TypeTable := TTypeTable.ttHolding;
    4: TypeTable := TTypeTable.ttInput;
    else
      Exit;
  end;

  // количество регистров для записи
  NRegs := aRequestPdu^[5] shr 1;

  // Начальное смещение в PDU
  PduOffset := 6;

  // Начальное смещение в Table
  TableOffset := aOffSet;

  // Таблица с данными
  Table := Lock(TypeTable);
  try

    while NRegs > 0 do
    begin

      // Слово из PDU
      Wrd := PWord(@aRequestPdu^[PduOffset])^;
      // Записать своппированное слово в Table
      Table^[TableOffset] := Swap(Wrd);

      // Счетчики
      Inc(PduOffset, 2);
      Inc(TableOffset, 1);
      Dec(NRegs, 1);
    end;

  finally
    Unlock;
  end;

end;


function TMap.ReadIoData(const aTypeTable: TTypeTable; const aOffSet: word;
  const aMultipler: dword): string;
type
  TIoData = record
    ioValue:  integer;
    ioStatus: byte;
    ioType:   byte;
  end;

var
  IoData: TIoData;
  Value:  dword;
  Table:  PTable;
  FoundIndex: Integer;
begin

  Table := Lock(aTypeTable);
  try
    Value := PDWord(@Table^[aOffSet])^;

    with IoData do
    begin
      ioValue  := Value and $FFFF;         // младшие 2 байта
      ioStatus := Value shr $10 and $00FF; // 3 байт
      ioType   := Value shr $F and $00FF;  // старший байт

      if ioType = 0 then
        ioValue := word(Value)
      else
        ioValue := smallint(Value);

      if aMultipler = 1 then
        fValue := IntToStr(ioValue)
      else
        fValue := FloatToStrF(ioValue / aMultipler, ffFixed, 15,
          NumberDecimals(aMultipler));

      FoundIndex := fStatus.IndexOf(IoStatus);
      if FoundIndex = -1 then
        fValue := Format('%s (не определено)', [fValue])
      else
        fValue := Format('%s (%s)', [fValue, fStatus.Data[FoundIndex].ShortDescription]);
    end;

  finally
    Unlock;
  end;

  Result := fValue;

end;

function TMap.ReadIpAddress(const aTypeTable: TTypeTable; const aOffSet: word): string;
var
  Table:      PTable;
  Value:      dword;
  a, b, c, d: byte;
begin

  // Таблица с данными
  Table := Lock(aTypeTable);
  try
    Value := PDWord(@Table^[aOffSet])^;

    d := Value and $FF;
    c := (Value shr 8) and $FF;
    b := (Value shr 16) and $FF;
    a := (Value shr 24) and $FF;

    fValue := Format('%d.%d.%d.%d', [a, b, c, d]);

  finally
    Unlock;
  end;

  Result := fValue;

end;

function TMap.ReadUint8l(const aTypeTable: TTypeTable; const aOffSet: word): byte;
var
  Table: PTable;
begin
  Result := 0;
  Table  := Lock(aTypeTable);
  try
    Result := Lo(Table^[aOffSet]);
  finally
    Unlock;
  end;

end;

function TMap.ReadUint8l(const aTypeTable: TTypeTable; const aOffSet: word;
  const aMultipler: dword): single;
begin
  Result :=  ReadUint8l(aTypeTable, aOffSet) / aMultipler;
end;

function TMap.ReadUint8h(const aTypeTable: TTypeTable; const aOffSet: word): byte;
var
  Table: PTable;
begin
  Result := 0;
  Table  := Lock(aTypeTable);
  try
    Result := Hi(Table^[aOffSet]);
  finally
    Unlock;
  end;
end;

function TMap.ReadUint8h(const aTypeTable: TTypeTable; const aOffSet: word;
  const aMultipler: dword): single;
begin
  Result :=  ReadUint8h(aTypeTable, aOffSet) / aMultipler;
end;

function TMap.ReadUint16(const aTypeTable: TTypeTable; const aOffSet: word): word;
var
  Table: PTable;
begin
  // Таблица с данными
  Table := Lock(aTypeTable);
  try
    Result := Table^[aOffSet]
  finally
    Unlock;
  end;
end;

function TMap.ReadUint16(const aTypeTable: TTypeTable; const aOffSet: word;
  const aMultipler: dword): single;
begin
  Result :=  ReadUint16(aTypeTable, aOffSet) / aMultipler;
end;

function TMap.ReadUint32(const aTypeTable: TTypeTable; const aOffSet: word
  ): dword;
var
  Table: PTable;
begin
  // Таблица с данными
  Table := Lock(aTypeTable);
  try
    Result := PDWord(@Table^[aOffSet])^;
  finally
    Unlock;
  end;
end;

function TMap.ReadUint32(const aTypeTable: TTypeTable; const aOffSet: word;
  const aMultipler: dword): single;
begin
  Result :=  ReadUint32(aTypeTable, aOffSet) / aMultipler;
end;

function TMap.ReadSint16(const aTypeTable: TTypeTable; const aOffSet: word
  ): smallInt;
var
  Table: PTable;
begin
  // Таблица с данными
  Table := Lock(aTypeTable);
  try
      Result := PSmallint(@Table^[aOffSet])^
  finally
    Unlock;
  end;
end;

function TMap.ReadSint16(const aTypeTable: TTypeTable; const aOffSet: word;
  const aMultipler: dword): single;
begin
  Result :=  ReadSint16(aTypeTable, aOffSet) / aMultipler;
end;

function TMap.ReadSint32(const aTypeTable: TTypeTable; const aOffSet: word
  ): longint;
var
  Table: PTable;
begin
  // Таблица с данными
  Table := Lock(aTypeTable);
  try
      Result := PLongint(@Table^[aOffSet])^;
  finally
    Unlock;
  end;
end;

function TMap.ReadSint32(const aTypeTable: TTypeTable; const aOffSet: word;
  const aMultipler: dword): single;
begin
  Result :=  ReadSint32(aTypeTable, aOffSet) / aMultipler;
end;

function TMap.ReadBitmap16(const aTypeTable: TTypeTable; const aOffSet: word
  ): word;
begin
  Result :=  ReadUint16(aTypeTable, aOffSet);
end;

function TMap.ReadBitmap32(const aTypeTable: TTypeTable; const aOffSet: word
  ): dword;
begin
  Result :=  ReadUint32(aTypeTable, aOffSet);
end;

function TMap.ReadFloat(const aTypeTable: TTypeTable; const aOffSet: word
  ): single;
var
  Table: PTable;
begin
  // Таблица с данными
  Table := Lock(aTypeTable);
  try
    Result := PSingle(@Table^[aOffSet])^;
  finally
    Unlock;
  end;
end;

function TMap.ReadBool(const aTypeTable: TTypeTable; const aOffSet: word
  ): boolean;
var
  Table: PTable;
begin
  // Таблица с данными
  Table := Lock(aTypeTable);
  try
    Result := boolean(Table^[aOffSet]);
  finally
    Unlock;
  end;
end;

procedure TMap.Clear;
var
  Table: PTable;
begin
  // Таблица с данными
  Table := Lock(ttInput);
  try
   ZeroMemory(Table, Length(Table^));
  finally
    Unlock;
  end;

  Table := Lock(ttHolding);
  try
   ZeroMemory(Table, Length(Table^));
  finally
    Unlock;
  end;
end;

end.
