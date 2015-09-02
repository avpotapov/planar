
unit uHoldingsData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLibrary,
  uMap,
  uHoldings;

type

  { THoldingsSaver }

  ITypeData = interface
    ['{F461FC21-75A7-4703-B387-01333D55F36C}']
    function GetValue: DWord;
    function GetValueStr: String;
    property Value: DWord read GetValue;
    property ValueStr: String read GetValueStr;
  end;

  { TTypeData }

  TTypeData = class(TInterfacedObject, ITypeData)
  protected
    fVarDefine: IVarDefine;
    fMap: IMap;
  protected
    function GetValue: DWord; virtual;
    function GetValueStr: String; virtual; abstract;
  public
    constructor Create(const aVarDefine: IVarDefine; const aMap: IMap); reintroduce;
  end;

  { TIntegerData }

  TIntegerData = class(TTypeData)

  protected
    function GetValueStr: String; override;
  end;

  { TSingleData }

  TSingleData = class(TTypeData)
  protected
    function GetValueStr: String; override;
  end;

  { TDataFactory }

  TDataFactory = class
  public
    class function GetTypeData(const aVarDefine: IVarDefine; const aMap: IMap): ITypeData;
  end;

  { THoldingBuilder }

  THoldingBuilder = class
  public
    class procedure BuildHolding(const aHolding: IHolding; const aVarDefine: IVarDefine; const aMap: IMap);
  end;


implementation

resourcestring
  SIntegerFormat = '%d';
  SBitmapFormat = '0x%.*x';
  SSingleFormat = '%.*f';


{ THoldingBuilder }

class procedure THoldingBuilder.BuildHolding(const aHolding: IHolding;
  const aVarDefine: IVarDefine; const aMap: IMap);
var
  TypeData: ITypeData;
begin
  TypeData := TDataFactory.GetTypeData(aVarDefine, aMap);

  aHolding.Name := aVarDefine.Name;
  aHolding.Index := aVarDefine.Index;
  aHolding.VarType := aVarDefine.VarType;
  aHolding.ShortDescription := aVarDefine.ShortDescription;
  aHolding.Multipler := aVarDefine.Multipler;
  aHolding.Value := TypeData.Value;
  aHolding.ValueStr := TypeData.ValueStr;
end;

{ TDataFactory }

class function TDataFactory.GetTypeData(const aVarDefine: IVarDefine;
  const aMap: IMap): ITypeData;
var
  IsSingle: Boolean;
begin
  IsSingle := (aVarDefine.VarType = TVarType.vtFLOAT) or (aVarDefine.Multipler > 1);
  if IsSingle then
    Result := TSingleData.Create(aVarDefine, aMap)
  else
    Result := TIntegerData.Create(aVarDefine, aMap);
end;

{ TTypeData }

constructor TTypeData.Create(const aVarDefine: IVarDefine; const aMap: IMap);
begin
  inherited Create;
  fVarDefine := aVarDefine;
  fMap := aMap;
end;

function TTypeData.GetValue: DWord;
begin
  if fVarDefine.VarType in DWordTypes then
    Result := fMap.ReadUint32(TTypeTable.ttHolding, fVarDefine.Index)
  else
    Result := fMap.ReadUint16(TTypeTable.ttHolding, fVarDefine.Index);
end;

{ TIntegerData }

function TIntegerData.GetValueStr: String;
begin
  Result := '0';
  case fVarDefine.VarType of
    vtUINT8L: Result   := Format(SIntegerFormat, [fMap.ReadUint8l(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtUINT8H: Result   := Format(SIntegerFormat, [fMap.ReadUint8h(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtUINT16: Result   := Format(SIntegerFormat, [fMap.ReadUint16(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtSINT16: Result   := Format(SIntegerFormat, [fMap.ReadSint16(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtUINT32: Result   := Format(SIntegerFormat, [fMap.ReadUint32(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtSINT32: Result   := Format(SIntegerFormat, [fMap.ReadSint32(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtBITMAP16: Result := Format( SBitmapFormat, [4, fMap.ReadBitmap16(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtBITMAP32: Result := Format( SBitmapFormat, [8, fMap.ReadBitmap32(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtBOOL_EXT: Result := Format(SIntegerFormat, [fMap.ReadUint32(TTypeTable.ttHolding, fVarDefine.Index)]);
    vtBOOL: Result     := BoolToStr(fMap.ReadBool(TTypeTable.ttHolding, fVarDefine.Index));
  end;
end;


{ TSingleData }

function TSingleData.GetValueStr: String;
var
  Precision: Byte;
begin
  Result := '0.0';
  Precision := NumberDecimals(fVarDefine.Multipler);

  case fVarDefine.VarType of
    vtUINT8l: Result := Format(SSingleFormat, [Precision, fMap.ReadUint8l(TTypeTable.ttHolding, fVarDefine.Index, fVarDefine.Multipler)]);
    vtUINT8H: Result := Format(SSingleFormat, [Precision, fMap.ReadUint8h(TTypeTable.ttHolding, fVarDefine.Index, fVarDefine.Multipler)]);
    vtUINT16: Result := Format(SSingleFormat, [Precision, fMap.ReadUint16(TTypeTable.ttHolding, fVarDefine.Index, fVarDefine.Multipler)]);
    vtSINT16: Result := Format(SSingleFormat, [Precision, fMap.ReadSint16(TTypeTable.ttHolding, fVarDefine.Index, fVarDefine.Multipler)]);
    vtUINT32: Result := Format(SSingleFormat, [Precision, fMap.ReadUint32(TTypeTable.ttHolding, fVarDefine.Index, fVarDefine.Multipler)]);
    vtSINT32: Result := Format(SSingleFormat, [Precision, fMap.ReadSint32(TTypeTable.ttHolding, fVarDefine.Index, fVarDefine.Multipler)]);
    vtFLOAT: Result := Format(SSingleFormat, [5, fMap.ReadFloat(TTypeTable.ttHolding, fVarDefine.Index)]);
  end;
end;






end.




