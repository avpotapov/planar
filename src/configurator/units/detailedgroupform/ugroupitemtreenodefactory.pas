unit uGroupItemTreeNodeFactory;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, VirtualTrees, Forms, Windows,
  uPropertyEditor,
  uLibrary,
  uMap,
  uModbus,
  uSetting;

type
  {$REGION Основные типы данных}

  // Базовый класс
  { TGroupItemData }

  TGroupItemData = class(TPropertyData)
  protected
    fReadOnly: Boolean;
  public
    procedure SetDescription(const {%H-}aStrings: TStrings); virtual;
    property ReadOnly: Boolean read fReadOnly;
  end;

  { TVarData }

  TVarData = class(TGroupItemData)
  protected
    fVarDefine: IVarDefine;
    fMap: IMap;
    fController: IController;
    fSlaveId: byte;
  protected
    function GetTable: TTypeTable;
    procedure WriteToDevice(const aFrame: IFrame);
  public
    constructor Create(const aVarDefine: IVarDefine; const aMap: IMap;
      const aController: IController; const aSlaveId: byte);
      virtual; reintroduce;
  public
    procedure SetDescription(const aStrings: TStrings); override;
    property VarDefine: IVarDefine read fVarDefine;
  end;

  { TStringData }

  TStringData = class(TVarData)
  protected
    function GetValue: String; override;
    procedure SetValue(const {%H-}aValue: String); override;
    public
    constructor Create(const aVarDefine: IVarDefine; const aMap: IMap;
      const aController: IController; const aSlaveId: byte); override;
  end;

  { TIntegerData }

  TIntegerData = class(TVarData)
  protected
    function GetValue: String; override;
    procedure SetValue(const aValue: String); override;
  public
    constructor Create(const aVarDefine: IVarDefine; const aMap: IMap;
      const aController: IController; const aSlaveId: byte); override;
  end;

  { TSingleData }

  TSingleData = class(TVarData)
  protected
    function GetValue: String; override;
    procedure SetValue(const aValue: String); override;
  public
    constructor Create(const aVarDefine: IVarDefine; const aMap: IMap;
      const aController: IController; const aSlaveId: byte); override;
  end;

  { TPicklistData }

  TPicklistData = class(TVarData)
  public
    constructor Create(const aVarDefine: IVarDefine; const aMap: IMap;
      const aController: IController; const aSlaveId: byte); override;
    procedure AfterConstruction; override;
  protected
    function GetValue: String; override;
    procedure SetValue(const aValue: String); override;
  public
    procedure SetDescription(const aStrings: TStrings); override;
  end;

  { TBitsData }

  TBitsData = class(TVarData)
  protected
    function GetValue: String; override;
    procedure SetValue(const {%H-}aValue: String); override;
  public
    constructor Create(const aVarDefine: IVarDefine; const aMap: IMap;
      const aController: IController; const aSlaveId: byte); override;
  end;

  { TBitData }

  TBitData = class(TGroupItemData)
  private
    fBitsData: TBitsData;
    fBit:      IBitDefine;
    fOwner:    PVirtualNode;
  protected
    function GetValue: String; override;
    procedure SetValue(const {%H-}aValue: String); override;
  public
    constructor Create(const aBit: IBitDefine; const aBitsData: TBitsData); reintroduce;
  public
    procedure SetDescription(const aStrings: TStrings); override;
    property Owner: PVirtualNode read fOwner write fOwner;
  end;

  {$ENDREGION Основные типы данных}

  {$REGION Фабрика узлов}

  { TGroupItemTreeNodeFactory }

  TGroupItemTreeNodeFactory = class
  private
    fTree: TBaseVirtualTree;
  public
    constructor Create(const aTree: TBaseVirtualTree); reintroduce;
  public
    function GetNode(const aVarDefine: IVarDefine; const aMap: IMap;
      const aController: IController; const aSlaveId: byte): PVirtualNode;
  end;

  {$ENDREGION Фабрика узлов}


implementation
//uses
//  Logger;

{$REGION Основные типы данных}
{ TGroupItemData }

procedure TGroupItemData.SetDescription(const aStrings: TStrings);
begin
  { TODO : Stub }
end;

{ TVarData }

function TVarData.GetTable: TTypeTable;
begin
  case fVarDefine.TypeRegister of
    trHolding: Result := TTypeTable.ttHolding;
    trInput: Result   := TTypeTable.ttInput;
  end;
end;

procedure TVarData.WriteToDevice(const aFrame: IFrame);
const
  MAX_COUNTER = 10;
var
  Counter: Integer;
begin
  {$IFDEF DEBUG}
  Assert(aFrame <> nil);
  Assert(fController <> nil);
  {$ENDIF}

  fMap.WriteToTable(fVarDefine.Index, aFrame.RequestPdu);

  aFrame.Priority := TPriority.prHigh;
  Counter := 0;

  while not fController.InQueue(aFrame) do
  begin
    if  Counter > MAX_COUNTER then
    begin
      MessageBox(0, PChar(Utf8ToAnsi('Ошибка записи в устройство')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
      Break;
    end;
    Application.ProcessMessages;
    sleep(10);
    Inc(Counter);
  end;


end;

constructor TVarData.Create(const aVarDefine: IVarDefine; const aMap: IMap;
  const aController: IController; const aSlaveId: byte);
var
  S: string;
begin
  if aVarDefine.Measure = '' then
    S := aVarDefine.ShortDescription
  else
    S := Format('%s, %s', [aVarDefine.ShortDescription, aVarDefine.Measure]);
  inherited Create(S, TTypeEditor.teEdit);

  fVarDefine := aVarDefine;
  fMap := aMap;
  fController := aController;
  fSlaveId := aSlaveId;
  fReadOnly := fVarDefine.TypeRegister = TTypeRegister.trInput;
end;

procedure TVarData.SetDescription(const aStrings: TStrings);
var
  C:     Char;
  Index: Integer;
  S:     String;

begin
    {$IFDEF DEBUG}
  Assert({%H-}aStrings <> nil);
    {$ENDIF}
  aStrings.BeginUpdate;
  try
    aStrings.Clear;

    if fVarDefine.TypeRegister = TTypeRegister.trHolding then
      C := 'h'
    else
      C := 'i';

    Index := VarTypes.IndexOfData(fVarDefine.VarType);

    aStrings.Add(Format('[%s %d] %s (%s)', [C, fVarDefine.Index,
      fVarDefine.ShortDescription, VarTypes.Keys[Index]]));

    if fVarDefine.Measure <> '' then
    begin
      aStrings.Add(' ');
      aStrings.Add(Format('Формат хранения: %s x %d', [fVarDefine.Measure, fVarDefine.Multipler]));
      aStrings.Add(' ');
    end;

    for S in fVarDefine.Description do
      aStrings.Add(S);



  finally
    aStrings.EndUpdate;
  end;
end;


{ TStringData }

function TStringData.GetValue: String;
begin
  case fVarDefine.VarType of
    vtUINTIP: Result  := fMap.ReadIpAddress(GetTable, fVarDefine.Index);
    vtIO_DATA: Result := fMap.ReadIoData(GetTable, fVarDefine.Index,
      fVarDefine.Multipler);
  end;
end;

procedure TStringData.SetValue(const aValue: String);
begin
  { TODO : Stub }
end;

constructor TStringData.Create(const aVarDefine: IVarDefine; const aMap: IMap;
  const aController: IController; const aSlaveId: byte);
begin
  inherited Create(aVarDefine, aMap, aController, aSlaveId);
end;

{ TIntegerData }

function TIntegerData.GetValue: String;
begin
  case fVarDefine.VarType of
    vtUINT8L: Result   := Format('%d', [fMap.ReadUint8l(GetTable, fVarDefine.Index)]);
    vtUINT8H: Result   := Format('%d', [fMap.ReadUint8h(GetTable, fVarDefine.Index)]);
    vtUINT16: Result   := Format('%d', [fMap.ReadUint16(GetTable, fVarDefine.Index)]);
    vtSINT16: Result   := Format('%d', [fMap.ReadSint16(GetTable, fVarDefine.Index)]);
    vtUINT32: Result   := Format('%d', [fMap.ReadUint32(GetTable, fVarDefine.Index)]);
    vtSINT32: Result   := Format('%d', [fMap.ReadSint32(GetTable, fVarDefine.Index)]);
    vtBOOL: Result     := BoolToStr(fMap.ReadBool(GetTable, fVarDefine.Index));
    vtBOOL_EXT: Result := Format('%d', [fMap.ReadUint32(GetTable, fVarDefine.Index)]);
  end;
end;

procedure TIntegerData.SetValue(const aValue: String);
var
  I: Integer;
  W: DWord;
  Frame: IFrame;

begin
  { TODO : StrToInt }
  I := StrToInt64Def(aValue, 0);

  case fVarDefine.VarType of
    vtUINT8L:
    begin

      W := (fMap.ReadUint16(GetTable, fVarDefine.Index) and $FF00) or I;
      Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, W, GetSetting.Timeout);
    end;
    vtUINT8H:
    begin
      W := fMap.ReadUint16(GetTable, fVarDefine.Index) and $FF or (I shl 8);
      Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, W, GetSetting.Timeout);
    end;
    vtUINT16: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, DWord(I), GetSetting.Timeout);
    vtSINT16: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, DWord(I), GetSetting.Timeout);
    vtUINT32: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
    vtSINT32: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
    vtFLOAT:  Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
  end;

  //Log.LogStatus(Frame.ToString, Self.ClassName);

  if Frame <> nil then
      WriteToDevice(Frame);

end;

constructor TIntegerData.Create(const aVarDefine: IVarDefine; const aMap: IMap;
  const aController: IController; const aSlaveId: byte);
begin
  inherited Create(aVarDefine, aMap, aController, aSlaveId);
  TypeEditor := TTypeEditor.teSpinEdit;
end;

{ TSingleData }

function TSingleData.GetValue: String;
begin
  case fVarDefine.VarType of
    vtUINT8L: Result := FloatToStrF(fMap.ReadUint8l(GetTable,
        fVarDefine.Index, fVarDefine.Multipler), ffFixed, 15,
        NumberDecimals(fVarDefine.Multipler));
    vtUINT8H: Result := FloatToStrF(fMap.ReadUint8h(GetTable,
        fVarDefine.Index, fVarDefine.Multipler), ffFixed, 15,
        NumberDecimals(fVarDefine.Multipler));
    vtUINT16: Result := FloatToStrF(fMap.ReadUint16(GetTable,
        fVarDefine.Index, fVarDefine.Multipler), ffFixed, 15,
        NumberDecimals(fVarDefine.Multipler));
    vtSINT16: Result := FloatToStrF(fMap.ReadSint16(GetTable,
        fVarDefine.Index, fVarDefine.Multipler),ffFixed, 15,
        NumberDecimals(fVarDefine.Multipler));
    vtUINT32: Result := FloatToStrF(fMap.ReadUint32(GetTable,
        fVarDefine.Index, fVarDefine.Multipler), ffFixed, 15,
        NumberDecimals(fVarDefine.Multipler));
    vtSINT32: Result := FloatToStrF(fMap.ReadSint32(GetTable,
        fVarDefine.Index, fVarDefine.Multipler), ffFixed, 15,
        NumberDecimals(fVarDefine.Multipler));
    vtFLOAT: Result  := FloatToStrF(fMap.ReadFloat(GetTable, fVarDefine.Index),
        ffGeneral, 15, 5);
  end;
end;

procedure TSingleData.SetValue(const aValue: String);
var
  F: Single;
  I: Int64;
  W: DWord;
  Frame: IFrame;

begin
  F := StrToFloatDef(aValue, 0);
  I := Round(F * fVarDefine.Multipler);

  case fVarDefine.VarType of
    vtUINT8L:
    begin
      W := (fMap.ReadUint16(GetTable, fVarDefine.Index) and $FF00) or I;
      Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, W, GetSetting.Timeout);
    end;
    vtUINT8H:
    begin
      W := fMap.ReadUint16(GetTable, fVarDefine.Index) and $FF or (I shl 8);
      Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, W, GetSetting.Timeout);
    end;
    vtUINT16: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, DWord(I), GetSetting.Timeout);
    vtSINT16: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, DWord(I), GetSetting.Timeout);
    vtUINT32: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
    vtSINT32: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
  end;

  if Frame <> nil then
      WriteToDevice(Frame);

end;

constructor TSingleData.Create(const aVarDefine: IVarDefine; const aMap: IMap;
  const aController: IController; const aSlaveId: byte);
begin
  inherited Create(aVarDefine, aMap, aController, aSlaveId);
  TypeEditor := TTypeEditor.teFloatSpinEdit;
end;

{ TPicklistData }

constructor TPicklistData.Create(const aVarDefine: IVarDefine;
  const aMap: IMap; const aController: IController; const aSlaveId: byte);
begin
  inherited Create(aVarDefine, aMap, aController, aSlaveId);
  TypeEditor := TTypeEditor.teComboBox;
end;

procedure TPicklistData.AfterConstruction;
var
  P: Pointer;
  PickItem: IPickItem;
  V: Word;
begin
  inherited AfterConstruction;
  {$IFDEF DEBUG}
  Assert(fVarDefine <> nil);
  Assert(Strings <> nil);
  {$ENDIF}
  for P in fVarDefine.Picklist do
  begin
    V := fVarDefine.Picklist.ExtractKey(P);
    PickItem := fVarDefine.Picklist.ExtractData(P);
    Strings.AddObject(Format('[%d] %s', [V, PickItem.ShortDescription]),
      TObject(Pointer(PickItem)));
  end;
end;

function TPicklistData.GetValue: String;
var
  I, FoundIndex: Integer;
begin

  I := 0;

  case fVarDefine.VarType of
    vtUINT8L: I := fMap.ReadUint8l(GetTable, fVarDefine.Index);
    vtUINT8H: I := fMap.ReadUint8h(GetTable, fVarDefine.Index);
    vtUINT16: I := fMap.ReadUint16(GetTable, fVarDefine.Index);
    vtSINT16: I := fMap.ReadSint16(GetTable, fVarDefine.Index);
    vtUINT32: I := fMap.ReadUint32(GetTable, fVarDefine.Index);
    vtSINT32: I := fMap.ReadSint32(GetTable, fVarDefine.Index);
  end;


  FoundIndex := fVarDefine.Picklist.IndexOf(I);
  if FoundIndex > -1 then
    Result := format('[%d] %s', [fVarDefine.Picklist.GetKey(FoundIndex),
      fVarDefine.Picklist.GetData(FoundIndex).ShortDescription])
  else
    Result := format('[%d] %s', [fVarDefine.Picklist.GetKey(0),
      fVarDefine.Picklist.GetData(0).ShortDescription]);

end;

procedure TPicklistData.SetValue(const aValue: String);
var
  FoundIndex: Integer;
  PickItem: IPickItem;
  I: Integer;
  W: DWord;
  Frame: IFrame;
begin
  FoundIndex := Strings.IndexOf(aValue);
  if FoundIndex < 0 then
    Exit;
  PickItem := IPickItem(Pointer(Strings.Objects[FoundIndex]));
  if PickItem = nil then
    Exit;
  I := PickItem.Value;

  case fVarDefine.VarType of
    vtUINT8L:
    begin

      W := (fMap.ReadUint16(GetTable, fVarDefine.Index) and $FF00) or I;
      Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, W, GetSetting.Timeout);
    end;
    vtUINT8H:
    begin
      W := fMap.ReadUint16(GetTable, fVarDefine.Index) and $FF or (I shl 8);
      Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, W, GetSetting.Timeout);
    end;
    vtUINT16: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, DWord(I), GetSetting.Timeout);
    vtSINT16: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 1, DWord(I), GetSetting.Timeout);
    vtUINT32: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
    vtSINT32: Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
    vtFLOAT:  Frame := WriteMultiple(fSlaveId, fVarDefine.Index, 2, DWord(I), GetSetting.Timeout);
  end;

  if Frame <> nil then
      WriteToDevice(Frame);

end;

procedure TPicklistData.SetDescription(const aStrings: TStrings);
var
  S: String;
begin
  inherited SetDescription(aStrings);

  aStrings.BeginUpdate;
  try
    aStrings.Add('Список значений:');
    for S in Strings do
      aStrings.Add(S);
  finally
    aStrings.EndUpdate;
  end;
end;

{ TBitsData }

function TBitsData.GetValue: String;
begin
  case fVarDefine.VarType of
    vtBITMAP16: Result := Format('0x%.4x',
        [fMap.ReadBitmap16(GetTable, fVarDefine.Index)]);
    vtBITMAP32: Result := Format('0x%.8x',
        [fMap.ReadBitmap32(GetTable, fVarDefine.Index)]);
  end;
end;

procedure TBitsData.SetValue(const aValue: String);
begin
  { TODO : Stub }
end;

constructor TBitsData.Create(const aVarDefine: IVarDefine; const aMap: IMap;
  const aController: IController; const aSlaveId: byte);
begin
  inherited Create(aVarDefine, aMap, aController, aSlaveId);
  fReadOnly := True;
end;

{ TBitData }

function TBitData.GetValue: String;
var
  W: Dword;
begin
  W := 0;

  case fBitsData.VarDefine.VarType of
    vtBITMAP16: W := fBitsData.fMap.ReadBitmap16(fBitsData.GetTable,
        fBitsData.VarDefine.Index);
    vtBITMAP32: W := fBitsData.fMap.ReadBitmap32(fBitsData.GetTable,
        fBitsData.VarDefine.Index);
  end;
  W      := W and (1 shl fBit.Index);
  Result := Format('0x%.2x', [W]);
  if W > 0 then
    fOwner^.CheckState := TCheckState.csCheckedNormal
  else
    fOwner^.CheckState := TCheckState.csUncheckedNormal;
end;

procedure TBitData.SetValue(const aValue: String);
var
  W: Dword;
  Frame: IFrame;
begin
  case fBitsData.VarDefine.VarType of
    vtBITMAP16:
    begin
      W := fBitsData.fMap.ReadBitmap16(fBitsData.GetTable,
        fBitsData.VarDefine.Index);
      W := W {%H-}xor (1 shl fBit.Index);
      Frame := WriteMultiple(fBitsData.fSlaveId, fBitsData.fVarDefine.Index, 1, W, GetSetting.Timeout);
    end;
    vtBITMAP32:
    begin
      W := fBitsData.fMap.ReadBitmap32(fBitsData.GetTable,
        fBitsData.VarDefine.Index);
      W := W {%H-}xor (1 shl fBit.Index);
      Frame := WriteMultiple(fBitsData.fSlaveId, fBitsData.fVarDefine.Index, 2, W, GetSetting.Timeout);
    end;
  end;

  if Frame <> nil then
      fBitsData.WriteToDevice(Frame);
end;

constructor TBitData.Create(const aBit: IBitDefine; const aBitsData: TBitsData);
begin
  inherited Create(aBit.ShortDescription);
  fBit      := aBit;
  fBitsData := aBitsData;
  fReadOnly := aBitsData.VarDefine.TypeRegister = TTypeRegister.trInput;
end;

procedure TBitData.SetDescription(const aStrings: TStrings);
var
  S: String;
begin
  {$IFDEF DEBUG}
  Assert({%H-}aStrings <> nil);
  {$ENDIF}
  aStrings.BeginUpdate;
  try
    aStrings.Clear;

    aStrings.Add(Format('[%d] %s',
      [fBit.Index, fBit.ShortDescription]));

    for S in fBit.Description do
      aStrings.Add(S);

  finally
    aStrings.EndUpdate;
  end;
end;

{$ENDREGION Основные типы данных}

{$REGION Фабрика узлов}

constructor TGroupItemTreeNodeFactory.Create(const aTree: TBaseVirtualTree);
begin
  inherited Create;
  fTree := aTree;
end;

function TGroupItemTreeNodeFactory.GetNode(const aVarDefine: IVarDefine;
  const aMap: IMap; const aController: IController; const aSlaveId: byte
  ): PVirtualNode;
type
  TTypeData = (tdPickList, tdBits, tdString, tdSingle, tdInteger);
var
  ConfigData: TGroupItemData;
  TypeData: TTypeData;
  P:    Pointer;
  Node: PVirtualNode;
  Bit:  IBitDefine;
  BitData: TBitData;
begin
  {$IFDEF DEBUG}
  Assert(fTree <> nil);
  {$ENDIF}

  TypeData := TTypeData.tdInteger;

  if aVarDefine.Picklist.Count > 0 then
    TypeData := TTypeData.tdPickList
  else
  if (aVarDefine.VarType = TVarType.vtBITMAP16) or
    (aVarDefine.VarType = TVarType.vtBITMAP32) then
    TypeData := TTypeData.tdBits
  else
  if aVarDefine.VarType = TVarType.vtIO_DATA then
    TypeData := TTypeData.tdString
  else
  if (aVarDefine.Multipler > 1) or (aVarDefine.VarType = TVarType.vtFLOAT) then
    TypeData := TTypeData.tdSingle;

  case TypeData of

    tdPickList:
    begin
      ConfigData := TPickListData.Create(aVarDefine, aMap, aController, aSlaveId);
      Result     := fTree.AddChild(nil, ConfigData);
    end;

    tdBits:
    begin
      ConfigData := TBitsData.Create(aVarDefine, aMap, aController, aSlaveId);
      Result     := fTree.AddChild(nil, ConfigData);
      // Биты
      for P in aVarDefine.Bits do
      begin
        Bit     := aVarDefine.Bits.ExtractData(P);
        BitData := TBitData.Create(Bit, ConfigData as TBitsData);
        Node    := fTree.AddChild(Result, BitData);
        Node^.CheckType := ctCheckBox;
        BitData.Owner := Node;
      end;
    end;

    tdString:
    begin
      ConfigData := TStringData.Create(aVarDefine, aMap, aController, aSlaveId);
      Result     := fTree.AddChild(nil, ConfigData);
    end;

    tdSingle:
    begin
      ConfigData := TSingleData.Create(aVarDefine, aMap, aController, aSlaveId);
      Result     := fTree.AddChild(nil, ConfigData);
    end;

    tdInteger:
    begin
      ConfigData := TIntegerData.Create(aVarDefine, aMap, aController, aSlaveId);
      Result     := fTree.AddChild(nil, ConfigData);
    end;
  end;

end;

{$ENDREGION Фабрика узлов}
end.
