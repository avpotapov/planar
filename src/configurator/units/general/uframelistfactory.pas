unit uFrameListFactory;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, fgl, uModbus, uLibrary, uSetting, uHoldings;

type
  {$REGION Фабрика списка фреймов Modbus}
  TVarList   = specialize TFpgList<IVarDefine>;
  TFrameList = specialize TFpgList<IFrame>;

  { TFrameListFactory }

  TFrameListFactory = class
  private
    fVarList: TVarList;

  private
    function GetFunctionCode(const aTypeRegister: TTypeRegister): byte;

  protected
    function GetOffSet(const aVarType: TVarType): byte;
    function GetNewFrame(const aSlaveId: Byte; const aTypeRegister: TTypeRegister;
      const aIndex: word; const aVarType: TVarType): IFrame;

  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure CreateFrameList(const aSlaveId: Byte; const aFrameList: TFrameList); virtual;
    property VarList: TVarList read fVarList;
  end;

  {$REGION Фабрика списка фреймов Modbus}


   { THoldingsWriterFrameListFactory }

   THoldingsWriterFrameListFactory = class(TFrameListFactory)
   private
     fHoldings: IHoldings;

   public
     procedure CreateFrameList(const aSlaveId: Byte; const aFrameList: TFrameList); override;
     property Holdings: IHoldings read fHoldings write fHoldings;
   end;




implementation

{ TFrameListFactory }

function Compare(const Item1, Item2: IVarDefine): Integer;
begin
  Result := Ord(Item1.TypeRegister) - Ord(Item2.TypeRegister);
  if Result = 0 then
    Result := Item1.Index - Item2.Index;
  if Result = 0 then
    Result := Ord(Item1.VarType) - Ord(Item2.VarType);
end;


function TFrameListFactory.GetOffSet(const aVarType: TVarType): byte;
begin
  Result := 1;
  if aVarType in DWordTypes then
    Result := 2;
end;

function TFrameListFactory.GetFunctionCode(const aTypeRegister: TTypeRegister): byte;
begin
  case aTypeRegister of
    trHolding: Result := 3;
    trInput:  Result := 4;
  end;
end;

function TFrameListFactory.GetNewFrame(const aSlaveId: Byte;
  const aTypeRegister: TTypeRegister; const aIndex: word;
  const aVarType: TVarType): IFrame;
begin
  case aTypeRegister of
  trHolding: Result := ReadHolding(aSlaveId, aIndex, GetOffSet(aVarType),
                                   GetSetting.Timeout);
  trInput: Result := ReadInput(aSlaveId, aIndex, GetOffSet(aVarType),
                                   GetSetting.Timeout);
  end;
end;

constructor TFrameListFactory.Create;
begin
  inherited Create;
  fVarList := TVarList.Create;
end;

destructor TFrameListFactory.Destroy;
begin
  FreeAndNil(fVarList);
  inherited Destroy;
end;


procedure TFrameListFactory.CreateFrameList(const aSlaveId: Byte;
  const aFrameList: TFrameList);
type
  TState = (sEmpty, sEqualFunctionCode, sSingleRequest, sOffSet, sMaxCount, sNewFrame);
  // Максимальное количество регистров в запросе
const
  MAX_SIZE = 120;
var
  State    : TState;
  VarDefine: IVarDefine;
  Count: Word;
begin
  {$IFDEF DEBUG}
  Assert(aFrameList <> nil);
  {$ENDIF}
  aFrameList.Clear;

  // Сортировать по правилу
  fVarList.Sort(@Compare);


  for VarDefine in fVarList do
  begin
    State := sEmpty;
    while True do

      case State of

        sEmpty:
          begin
            if aFrameList.Count = 0 then
              State := sNewFrame
            else
              State := sEqualFunctionCode;
          end;

        sEqualFunctionCode:
          begin
            if aFrameList.Last.RequestPdu^[0] = GetFunctionCode(VarDefine.TypeRegister) then
              State := sSingleRequest
            else
              State := sNewFrame;
          end;

        sSingleRequest:
          begin
            if VarDefine.SingleRequest then
              State := sNewFrame
            else
              State := sOffSet;
          end;

        sOffSet:
          begin
            if VarDefine.Index < Swap(PWord(@aFrameList.Last.RequestPdu^[1])^) +
               Swap(PWord(@aFrameList.Last.RequestPdu^[3])^) then
              Break // это 8-битная переменная
            else if VarDefine.Index = Swap(PWord(@aFrameList.Last.RequestPdu^[1])^) +
                    Swap(PWord(@aFrameList.Last.RequestPdu^[3])^) then
              State := sMaxCount
            else
              State := sNewFrame;
          end;

        sMaxCount:
          begin

            Count := Swap(PWord(@aFrameList.Last.RequestPdu^[3])^) + GetOffSet(VarDefine.VarType);
            if  Count > MAX_SIZE then
              State := sNewFrame
            else
            begin
              aFrameList.Last.RequestPdu^[3] := Hi(Count);
              aFrameList.Last.RequestPdu^[4] := Lo(Count);

              Break;
            end;
          end;

        sNewFrame:
          begin
            aFrameList.Add(GetNewFrame(aSlaveId, VarDefine.TypeRegister, VarDefine.Index, VarDefine.VarType));
            Break;
          end;

      end;
  end;


end;


function CompareHoldings(const Item1, Item2: IHolding): Integer;
begin
  Result := Item1.Index - Item2.Index;
  if Result = 0 then
    Result := Ord(Item1.VarType) - Ord(Item2.VarType);
end;

{ THoldingsWriterFrameListFactory }


procedure THoldingsWriterFrameListFactory.CreateFrameList(const aSlaveId: Byte;
  const aFrameList: TFrameList);
var
  Frame: IFrame;
  Holding: IHolding;
begin
  {$IFDEF DEBUG}
  Assert(aFrameList <> nil);
  {$ENDIF}
  aFrameList.Clear;
  fHoldings.Sort(@CompareHoldings);
  if fHoldings = nil then
    Exit;
  for Holding in fHoldings do
  begin
    if aFrameList.Count > 0 then
     // Убираем дубликат запроса
     if Swap(PWord(@aFrameList.Last.RequestPdu^[1])^) = Holding.Index then
        Continue;
    Frame := WriteMultiple(aSlaveId, Holding.Index, GetOffSet(Holding.VarType), Holding.Value, GetSetting.Timeout);
    aFrameList.Add(Frame);
  end;


end;


end.
