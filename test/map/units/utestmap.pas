unit utestmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umap, umapcls, fpcunit, testregistry, windows;

type

  { TTestMap }

  TTestMap= class(TTestCase)

  published
    procedure TestInstantiation;
    procedure TestWrite;
    procedure TestWrite1;
    procedure TestReadUint8l;
    procedure TestIoData;
    procedure TestBitmap;
    procedure TestIpAddress;
    procedure TestFloat;
    procedure TestReadUint8l_;
    procedure TestReadBool_;
    procedure TestDecimal;
  end;

implementation

procedure TTestMap.TestInstantiation;
begin
  AssertTrue(GetMap <> nil);
end;

procedure TTestMap.TestWrite;
var
  Pdu: TPdu;
  Map: IMap;
  T: PTable;
  Wrd, Wrd1: word;
begin
  Pdu[0] := 3;
  Pdu[1] := 6;   // 3 регистрам
  Pdu[2] := $EF;
  Pdu[3] := $EE;
  Pdu[4] := $DD;
  Pdu[5] := $CC;
  Pdu[6] := $BB;
  Pdu[7] := $AA;

  Map := TMap.Create;
  Map.WriteData(0, @Pdu);

  T := Map.Lock(TTypeTable.ttHolding);
  try
    Wrd := 0;
    Wrd := Pdu[2] shl 8 or Pdu[3];
    Wrd1 := Swap(PWord(@Pdu[2])^);

    Writeln(IntToHex(Wrd, 4));
    Writeln(IntToHex(Wrd1, 4));

    Writeln(IntToHex(T^[0], 4));
    Writeln(IntToHex(T^[1], 4));
    Writeln(IntToHex(T^[2], 4));
    Writeln(IntToHex(T^[3], 4));
  finally
    Map.UnLock;
  end;
end;

procedure TTestMap.TestWrite1;
var
  Pdu: TPdu;
  Map: IMap;
  T: PTable;
  Wrd, Wrd1: word;
begin
  Pdu[0] := 4;
  Pdu[1] := 2;   // 1 регистр
  Pdu[2] := $EF;
  Pdu[3] := $EE;


  Map := GetMap;
  Map.WriteData(0, @Pdu);

  T := Map.Lock(TTypeTable.ttInput);
  try
    Wrd := 0;
    Wrd := Pdu[2] shl 8 or Pdu[3];
    Wrd1 := Swap(PWord(@Pdu[2])^);

    Writeln(IntToHex(Wrd, 4));
    Writeln(IntToHex(Wrd1, 4));

    Writeln(IntToHex(T^[0], 4));
    Writeln(IntToHex(T^[1], 4));
    Writeln(IntToHex(T^[2], 4));
    Writeln(IntToHex(T^[3], 4));
  finally
    Map.UnLock;
  end;

end;

procedure TTestMap.TestReadUint8l;
var
  Pdu: TPdu;
  T: PTable;
  Map: IMap;
  B: byte;
  W: word;
  DW: dword;
  S:  smallint;
  SS: longInt;
begin
  Pdu[0] := 4;
  Pdu[1] := 4;   // 2 регистр
  Pdu[2] := $AA;
  Pdu[3] := $BB;
  Pdu[4] := $CC;
  Pdu[5] := $DD;
  Pdu[6] := $EE;
  Pdu[7] := $FF;

  Map := GetMap;//TMap.Create;

  Map.WriteData(0, @Pdu);

  Writeln('-----------------------------------------');
  B := StrToInt(Map.ReadUint8l(TTypeTable.ttInput, 0));
  Writeln(IntToHex(B, 2));

  B := StrToInt(Map.ReadUint8h(TTypeTable.ttInput, 0));
  Writeln(IntToHex(B, 2));

  DW := StrToInt64(Map.ReadUint32(TTypeTable.ttInput, 0));
  Writeln(IntToHex(DW, 8));

  DW := StrToInt64(Map.ReadUint32(TTypeTable.ttInput, 0));
  Writeln(IntToHex(DW, 8));

  s := StrToInt(Map.ReadSUint16(TTypeTable.ttInput, 0));
  Writeln(s);

  ss := StrToInt64(Map.ReadSUint32(TTypeTable.ttInput, 0));
  Writeln(ss);
end;

procedure TTestMap.TestIoData;
var
  Pdu: TPdu;
  Map: IMap;

begin
  Pdu[0] := 4;
  Pdu[1] := 4;   // 2 регистр
  Pdu[2] := $10;
  Pdu[3] := $00;
  Pdu[4] := $11;
  Pdu[5] := $1;
  Pdu[6] := $EE;
  Pdu[7] := $FF;

  Map := GetMap;//TMap.Create;

  Map.WriteData(0, @Pdu);

  Writeln('-----------------------------------');
  Writeln(Map.ReadIoData(TTypeTable.ttInput, 0));

end;

procedure TTestMap.TestBitmap;
var
  Pdu: TPdu;
  Map: IMap;
begin
  Pdu[0] := 4;
  Pdu[1] := 6;   // 2 регистр
  Pdu[2] := $AA;
  Pdu[3] := $BB;
  Pdu[4] := $CC;
  Pdu[5] := $DD;
  Pdu[6] := $EE;
  Pdu[7] := $FF;

  Map := GetMap;//TMap.Create;

  Map.WriteData(0, @Pdu);
  Writeln('-- Bitmap --');
  Writeln(Map.ReadBitmap16(TTypeTable.ttInput, 0));

end;

procedure TTestMap.TestIpAddress;
var
  Pdu: TPdu;
  Map: IMap;
begin
  Pdu[0] := 4;
  Pdu[1] := 6;   // 2 регистр
  Pdu[2] := $AA;
  Pdu[3] := $BB;
  Pdu[4] := $CC;
  Pdu[5] := $DD;
  Pdu[6] := $EE;
  Pdu[7] := $FF;

  Map := GetMap;//TMap.Create;

  Map.WriteData(0, @Pdu);
  Writeln('-- IpAddress --');
  Writeln(Map.ReadIpAddress(TTypeTable.ttInput, 0));


end;

procedure TTestMap.TestFloat;
var
  Pdu: TPdu;
  Map: IMap;
begin
  Pdu[0] := 4;
  Pdu[1] := 6;   // 2 регистр
  Pdu[2] := $11;
  Pdu[3] := $22;
  Pdu[4] := $33;
  Pdu[5] := $44;
  Pdu[6] := $EE;
  Pdu[7] := $FF;

  Map := GetMap;//TMap.Create;
  Map.WriteData(0, @Pdu);

  Writeln('-- Float --');
  Writeln(Map.ReadFloat(TTypeTable.ttInput, 0));

end;

procedure TTestMap.TestReadUint8l_;
var
  Pdu: TPdu;
  Map: IMap;
begin
  Pdu[0] := 4;
  Pdu[1] := 6;   // 2 регистр
  Pdu[2] := $11;
  Pdu[3] := $22;
  Pdu[4] := $33;
  Pdu[5] := $44;
  Pdu[6] := $EE;
  Pdu[7] := $FF;

  Map := GetMap;//TMap.Create;
  Map.WriteData(0, @Pdu);

  Writeln('-- Uint8l --');
  Writeln(Map._ReadUint8l(TTypeTable.ttInput, 0));

  Writeln('-- Uint8lSingle --');
  Writeln(Map._ReadUint8l(TTypeTable.ttInput, 0, 100));
end;

procedure TTestMap.TestReadBool_;
var
  Pdu: TPdu;
  Map: IMap;
begin
  Pdu[0] := 4;
  Pdu[1] := 6;   // 3 регистр
  Pdu[2] := $0;
  Pdu[3] := $0;
  Pdu[4] := $33;
  Pdu[5] := $44;
  Pdu[6] := $EE;
  Pdu[7] := $FF;

  Map := GetMap;//TMap.Create;
  Map.WriteData(0, @Pdu);

  Writeln('-- Bool --');
  Writeln(Map._ReadBool(TTypeTable.ttInput, 0));
  Writeln(Map._ReadBool(TTypeTable.ttInput, 1));
end;

procedure TTestMap.TestDecimal;
begin
//  AssertTrue(NumberDecimals(10)=1);
  Writeln(NumberDecimals(1));
  Writeln(NumberDecimals(10));
  WriteLn(NumberDecimals(100));
  WriteLn(NumberDecimals(1000));
  WriteLn(NumberDecimals(10000));

//  AssertTrue(NumberDecimals(100)=2);

end;



initialization

  RegisterTest(TTestMap);
end.


