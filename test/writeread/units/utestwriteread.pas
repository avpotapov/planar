unit uTestWriteRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umap, umodbus, fpcunit, testregistry, Windows;

type

  { TTestWriteRead }

  TTestWriteRead= class(TTestCase)
  published
    procedure TestUInt8h;
    procedure TestUInt8hMin;
    procedure TestUInt8hMax;
    procedure TestUInt8l;
    procedure TestUInt8lMin;
    procedure TestUInt8lMax;
    procedure TestUInt16;
    procedure TestUInt16Min;
    procedure TestUInt16Max;
    procedure TestUInt32;
    procedure TestUInt32Min;
    procedure TestUInt32Max;
    procedure TestSInt16;
    procedure TestSInt16Min;
    procedure TestSInt16Max;
    procedure TestSInt32;
    procedure TestSInt32Min;
    procedure TestSInt32Max;
    procedure TestSInt16_;
    procedure TestSInt32_;
    procedure TestFloat;
    procedure TestMulti10;
    procedure TestMulti100;
    procedure TestMulti1000;
    procedure TestMulti10000;
    procedure TestMulti100000;
    procedure TestMulti10_;
    procedure TestMulti100_;
    procedure TestMulti1000_;
    procedure TestMulti10000_;
    procedure TestMulti100000_;

   end;

implementation
var
  Controller: IController;
  Map: IMap;

{ TTestWriteRead }

procedure TTestWriteRead.TestUInt8h;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Byte = $12;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Byte;
begin
  Writeln('------------ UInt8h ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue shl 8, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint8h(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);
end;

procedure TTestWriteRead.TestUInt8hMin;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Byte = $0;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Byte;
begin
  Writeln('------------ UInt8hMin ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue shl 8, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint8h(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt8hMax;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Byte = $FF;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Byte;
begin
  Writeln('------------ UInt8hMax ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue shl 8, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint8h(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt8l;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Byte = $34;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Byte;
begin
  Writeln('------------ UInt8l ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint8l(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt8lMin;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Byte = $0;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Byte;
begin
  Writeln('------------ UInt8lMin ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint8l(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt8lMax;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Byte = $FF;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Byte;
begin
  Writeln('------------ UInt8lMax ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint8l(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt16;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Word = $1234;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Word;
begin
  Writeln('------------ UInt16 ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint16(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);
end;

procedure TTestWriteRead.TestUInt16Min;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Word = $0;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Word;
begin
  Writeln('------------ UInt16Min ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint16(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt16Max;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: Word = $FFFF;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: Word;
begin
  Writeln('------------ UInt16Max ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint16(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt32;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: DWord = $12345678;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: DWord;
begin
  Writeln('------------ UInt32 ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint32(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);
end;

procedure TTestWriteRead.TestUInt32Min;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: DWord = $0;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: DWord;
begin
  Writeln('------------ UInt32Min ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint32(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestUInt32Max;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: DWord = $FFFFFFFF;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: DWord;
begin
  Writeln('------------ UInt32Max ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadUint32(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestSInt16;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: SmallInt = $1234;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: SmallInt;
begin
  Writeln('------------ SInt16 ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt16(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestSInt16_;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: SmallInt = $D678;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: SmallInt;
begin
  Writeln('------------ SInt16_ -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt16(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);
end;

procedure TTestWriteRead.TestSInt16Min;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: SmallInt = -32768;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: SmallInt;
begin
  Writeln('------------ SInt16Min ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt16(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestSInt16Max;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 1;
  TestValue: SmallInt = 32767;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: SmallInt;
begin
  Writeln('------------ SInt16Max ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt16(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestSInt32;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: LongInt = $12345678;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: LongInt;
begin
  Writeln('------------ SInt32 ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;
procedure TTestWriteRead.TestSInt32_;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: LongInt = $8044812D;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: LongInt;
begin
  Writeln('------------ SInt32_ -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;
procedure TTestWriteRead.TestSInt32Min;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: LongInt = -2147483648;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: LongInt;
begin
  Writeln('------------ SInt32Min ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestSInt32Max;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: LongInt = 2147483647;
  FormatStr = 'Value: 0x%x';
var
  Frame: IFrame;
  ReturnValue: LongInt;
begin
  Writeln('------------ SInt32Max ------------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, TestValue, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format(FormatStr, [TestValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format(FormatStr, [ReturnValue]));
  Writeln(Format('Value %d', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;


procedure TTestWriteRead.TestFloat;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = 3.1415266;
var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ Float -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(TestValue), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %.5f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadFloat(TTypeTable.ttHolding, OffSet);

  Writeln('READ');
  Writeln(Format('Value %.5f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);


end;

procedure TTestWriteRead.TestMulti10;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = 123.4;
  Multipler: DWord = 10;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32 / 10 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti100;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = 123.45;
  Multipler: DWord = 100;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32 / 100 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti1000;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = 123.456;
  Multipler: DWord = 1000;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32 / 1000 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %.3f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %.3f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti10000;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = 123.4567;
  Multipler: DWord = 10000;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32 / 10000 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %.4f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %.4f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti100000;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = 123.45678;
  Multipler: DWord = 100000;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32 / 100000 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %.5f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %.5f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti10_;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = -123.4;
  Multipler: DWord = 10;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32_ / 10 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti100_;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = -123.45;
  Multipler: DWord = 100;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32_ / 100 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti1000_;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = -123.456;
  Multipler: DWord = 1000;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32_ / 1000 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %.3f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %.3f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti10000_;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = -123.4567;
  Multipler: DWord = 10000;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32_ / 10000 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %.4f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %.4f', [ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;

procedure TTestWriteRead.TestMulti100000_;
const
  OffSet: Word = 0;
  SlaveId: Byte = 1;
  RegCount: Word = 2;
  TestValue: Single = -123.45678;
  Multipler: DWord = 100000;

var
  Frame: IFrame;
  ReturnValue: Single;
begin
  Writeln('------------ SInt32_ / 100000 -----------');

  Frame := WriteMultiple(SlaveId, OffSet, RegCount, DWord(Round(TestValue * Multipler)), 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Writeln('WRITE');
  Writeln(Format('Value %.5f', [TestValue]));
  Writeln(Utf8Decode(Frame.ToString));

  Frame := ReadHolding(SlaveId, OffSet, RegCount, 300);
  AssertTrue(Controller.InQueue(Frame));
  AssertTrue(Frame.Responded);

  Map.WriteData(OffSet, Frame.ResponsePdu);
  ReturnValue := Map.ReadSUInt32(TTypeTable.ttHolding, OffSet, Multipler);

  Writeln('READ');
  Writeln(Format('Value %.5f', [ReturnValue]));
   Writeln(Format('Value %.*f', [4, ReturnValue]));

  Writeln(Utf8Decode(Frame.ToString));

  AssertEquals(TestValue, ReTurnValue);

end;




initialization
  SetConsoleCp(1251);
  SetConsoleOutputCP(1251);
//  Controller :=  GetRtuBuilder.GetController('COM3', 19200, 2, 0, 8, 30);
  Controller := GetTcpBuilder.GetController('192.168.0.166', 502);
  Controller.Open;
  Map := GetMap;

  RegisterTest(TTestWriteRead);
end.

