unit uframefactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uframe,
  uModbus;

type

  { TFrameFactory }

  TFrameFactory = class
  private
    class function GetStandartFrame(const aSlaveId, aFunctionCode: byte;
      const aStart, aRegCount: word; const aTimeout: dword): IFrame;
  public
    class function ReadInput(const aSlaveId: byte; const aStart,
      aRegCount: word; const aTimeout: dword): IFrame;

    class function ReadHolding(const aSlaveId: byte; const aStart,
      aRegCount: word; const aTimeout: dword): IFrame;

    class function WriteMultiple(const aSlaveId: byte;
      const aStart, aRegCount: word; const aValue, aTimeout: dword): IFrame;

    class function WriteStruct(const aSlaveId: byte; const aPdu: TBuffer; const aCount: word; const aTimeout: dword): IFrame;

    class function RunApplication(const aSlaveId: byte;
      const aTimeout: dword): IFrame;

    class function ResetApplication(const aSlaveId: byte;
      const aTimeout: dword): IFrame;

    class function ReadSerial(const aSlaveId: byte;
      const aTimeout: dword): IFrame;

    class function RunBootloader(const aSlaveId: byte;
      const aTimeout: dword): IFrame;

    class function WritePage(const aSlaveId: byte;
      const aCurrentPage, aSize: word; const aBuffer: PBuffer;
      const aTimeout: dword): IFrame;


  end;

implementation

const

  FC_READ_INPUT = 4;
  FC_READ_HOLDING = 3;
  FC_WRITE_MULTIPLE = 16;

{ TFrameFactory }

class function TFrameFactory.GetStandartFrame(
  const aSlaveId, aFunctionCode: byte; const aStart, aRegCount: word;
  const aTimeout: dword): IFrame;
begin
  Result := TFrame.Create;
  Result.SlaveId := aSlaveId;

  Result.RequestPdu^[0] := aFunctionCode;
  Result.RequestPdu^[1] := Hi(aStart);
  Result.RequestPdu^[2] := Lo(aStart);
  Result.RequestPdu^[3] := Hi(aRegCount);
  Result.RequestPdu^[4] := Lo(aRegCount);

  Result.RequestCount := 5;

  Result.Timeout := aTimeout;

end;

class function TFrameFactory.ReadInput(const aSlaveId: byte;
  const aStart, aRegCount: word; const aTimeout: dword): IFrame;
begin
  Result := GetStandartFrame(aSlaveId, FC_READ_INPUT,
    aStart, aRegCount, aTimeout);
end;

class function TFrameFactory.ReadHolding(const aSlaveId: byte;
  const aStart, aRegCount: word; const aTimeout: dword): IFrame;
begin
  Result := GetStandartFrame(aSlaveId, FC_READ_HOLDING,
    aStart, aRegCount, aTimeout);
end;

class function TFrameFactory.WriteMultiple(const aSlaveId: byte;
  const aStart, aRegCount: word; const aValue, aTimeout: dword): IFrame;
var
  I: byte;
  ValueSize: byte;
  PValue: pbyte;

begin
  Result := GetStandartFrame(aSlaveId, FC_WRITE_MULTIPLE,
    aStart, aRegCount, aTimeout);

  // Количество байт данных
  ValueSize := 2 * aRegCount;
  Result.RequestPdu^[5] := ValueSize;


  // Указатель на 1 байт данных
  PValue := @aValue;
  I := 0;
  while I <= (aRegCount) do
  begin

    PValue := PValue + I;

    Result.RequestPdu^[6 + I] := Hi(PWord(PValue)^);
    Result.RequestPdu^[6 + I + 1] := Lo(PWord(PValue)^);

    Inc(I, 2);
  end;

  Result.RequestCount := 6 + ValueSize;
end;

class function TFrameFactory.WriteStruct(const aSlaveId: byte; const aPdu: TBuffer;
  const aCount: word; const aTimeout: dword): IFrame;
begin
  Result := TFrame.Create;
  Result.SlaveId:= aSlaveId;;
  System.Move(aPdu[0], Result.RequestPdu^[0], aCount);
  Result.RequestCount := aCount;
  Result.Timeout := aTimeout;
end;

class function TFrameFactory.RunApplication(const aSlaveId: byte;
  const aTimeout: dword): IFrame;
const
  MB_DEVICE_FUNCTION: Byte = $68;
  MB_RUN_APPLICATION: Byte = 1;
  PROTECT_CODE: Dword = $5FA0A05F;

  SizeOfBuffer: Byte  = 7;

begin
  Result := TFrame.Create;
  Result.SlaveId := aSlaveId;

  Result.RequestPdu^[0] := MB_DEVICE_FUNCTION;
  Result.RequestPdu^[1] := MB_RUN_APPLICATION;
  Result.RequestPdu^[2] := 0;
  PDWord(@Result.RequestPdu^[3])^ := PROTECT_CODE;

  Result.RequestCount := SizeOfBuffer;

  Result.Timeout := aTimeout;
end;

class function TFrameFactory.ResetApplication(const aSlaveId: byte;
  const aTimeout: dword): IFrame;
const
  MB_DEVICE_FUNCTION: Byte = $44;
  MB_RESET_APPLICATION: Byte  = 4;
  PROTECT_CODE: Dword = $5FA0A05F;

  SizeOfBuffer: Byte  = 7;

begin
  Result := TFrame.Create;
  Result.SlaveId := aSlaveId;

  Result.RequestPdu^[0] := MB_DEVICE_FUNCTION;
  Result.RequestPdu^[1] := MB_RESET_APPLICATION;
  Result.RequestPdu^[2] := 0;
  PDWord(@Result.RequestPdu^[3])^ := PROTECT_CODE;

  Result.RequestCount := SizeOfBuffer;

  Result.Timeout := aTimeout;

end;

class function TFrameFactory.ReadSerial(const aSlaveId: byte;
  const aTimeout: dword): IFrame;
const
  MB_DEVICE_FUNCTION: Byte = 71;
  MB_READ_SERIAL: Byte  = 0;

  SizeOfBuffer: Byte  = 2;

begin
  Result := TFrame.Create;
  Result.SlaveId := aSlaveId;

  Result.RequestPdu^[0] := MB_DEVICE_FUNCTION;
  Result.RequestPdu^[1] := MB_READ_SERIAL;


  Result.RequestCount := SizeOfBuffer;

  Result.Timeout := aTimeout;

end;

class function TFrameFactory.RunBootloader(const aSlaveId: byte;
  const aTimeout: dword): IFrame;
const
  MB_DEVICE_FUNCTION: Byte = $68;
  MB_RUN_BOOTLOADER: Byte  = 2;
  PROTECT_CODE: Dword = $5FA0A05F;

  SizeOfBuffer: Byte  = 7;

begin
  Result := TFrame.Create;
  Result.SlaveId := aSlaveId;

  Result.RequestPdu^[0] := MB_DEVICE_FUNCTION;
  Result.RequestPdu^[1] := MB_RUN_BOOTLOADER;
  Result.RequestPdu^[2] := 0;
  PDWord(@Result.RequestPdu^[3])^ := PROTECT_CODE;

  Result.RequestCount := SizeOfBuffer;

  Result.Timeout := aTimeout;

end;

class function TFrameFactory.WritePage(const aSlaveId: byte;
  const aCurrentPage, aSize: word; const aBuffer: PBuffer;
  const aTimeout: dword): IFrame;
const
  MB_FLASH_FUNCTION: Byte = $64;
  MB_WRITE_FLASH: Byte    = 1;
  SizeOfBuffer: Byte  = 5;
begin
  Result := TFrame.Create;
  Result.SlaveId := aSlaveId;

  Result.RequestPdu^[0] := MB_FLASH_FUNCTION;
  Result.RequestPdu^[1] := MB_WRITE_FLASH;
  Result.RequestPdu^[2] := Lo(aCurrentPage);
  Result.RequestPdu^[3] := Hi(aCurrentPage);
  Result.RequestPdu^[4] := aSize;
  System.Move(aBuffer^[0],  Result.RequestPdu^[5], aSize);


  Result.RequestCount := SizeOfBuffer + aSize;

  Result.Timeout := aTimeout;
end;

end.














