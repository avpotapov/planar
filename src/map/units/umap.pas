unit uMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

const
  MINWORD     = 0;
  BUFFER_SIZE = 1024;


type
  TBuffer = array[0 .. BUFFER_SIZE - 1] of byte;
  PBuffer = ^TBuffer;
  TPdu    = TBuffer;
  PPdu    = ^TPdu;


  TTable     = array [MINWORD .. MAXWORD] of word;
  PTable     = ^TTable;
  TTypeTable = (ttHolding, ttInput);
  TTables    = array [ttHolding .. ttInput] of TTable;


  IBaseMap = interface
    ['{DA9DDABD-10F3-4F91-A9C8-32BA4A0016DA}']
    function Lock(const aTypeTable: TTypeTable = TTypeTable.ttInput): PTable;
    procedure UnLock;
  end;

  { IMap }

  IMap = interface(IBaseMap)
    ['{B239D504-0D7F-4BDB-9ECC-6AB304425BF0}']
    procedure WriteToTable(const aOffSet: word; const aRequestPdu: PPdu);
    procedure WriteData(const aOffSet: word; const aPdu: PPdu);

    function ReadIpAddress(const aTypeTable: TTypeTable; const aOffSet: word): string;
    function ReadIoData(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword = 1): string;


    function ReadUint8l(const aTypeTable: TTypeTable; const aOffSet: word): byte;
    function ReadUint8l(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;

    function ReadUint8h(const aTypeTable: TTypeTable; const aOffSet: word): byte;
    function ReadUint8h(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;

    function ReadUint16(const aTypeTable: TTypeTable; const aOffSet: word): word;
    function ReadUint16(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;

    function ReadUint32(const aTypeTable: TTypeTable; const aOffSet: word): dword;
    function ReadUint32(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;

    function ReadSint16(const aTypeTable: TTypeTable; const aOffSet: word): smallint;

    function ReadSint16(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;

    function ReadSint32(const aTypeTable: TTypeTable; const aOffSet: word): longint;
    function ReadSint32(const aTypeTable: TTypeTable; const aOffSet: word;
      const aMultipler: dword): single;

    function ReadBitmap16(const aTypeTable: TTypeTable; const aOffSet: word): word;
    function ReadBitmap32(const aTypeTable: TTypeTable; const aOffSet: word): dword;

    function ReadFloat(const aTypeTable: TTypeTable; const aOffSet: word): single;

    function ReadBool(const aTypeTable: TTypeTable; const aOffSet: word): boolean;

    procedure Clear;


  end;




function GetMap: IMap; external 'map.dll';

function NumberDecimals(const aMultipler: dword): byte;

implementation

function NumberDecimals(const aMultipler: dword): byte;
var
  Value: dword;
begin
  // Множитель
  Value  := aMultipler;
  Result := 0;
  // Количество разрядов множителя 10 - 1, 100 - 2, 1000 - 3
  while Value > 1 do
  begin
    Value := Value div 10;
    Inc(Result);
  end;
end;

end.
