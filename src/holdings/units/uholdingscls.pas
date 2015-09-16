unit uHoldingsCls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  uLibrary,
  uHoldings{, uString};

type

  THoldingsSpec = specialize TFpgList<IHolding>;

  THoldingsEnumeratorSpec = specialize TFpgListEnumerator<IHolding>;

  { THoldingsEnumerator }

  THoldingsEnumerator = class(THoldingsEnumeratorSpec, IHoldingsEnumerator)
  protected
    fRefCount : longint;
  protected
    function QueryInterface(constref Iid : TGuid; out Obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
  public
    procedure AfterConstruction; override;
    property RefCount : longint read fRefCount;
end;

  { THoldings }

  THoldings = class(THoldingsSpec, IHoldings)
  private
    fCreated: TDateTime;
    fModuleName: string;
    fUid: word;

  protected
    function GetCreated: TDateTime;
    function GetModuleName: string;
    function GetUid: word;
    procedure SetCreated(const aCreated: TDateTime);
    procedure SetModuleName(const aModuleName: string);
    procedure SetUid(const aUid: word);
  protected
    fRefCount : longint;
  protected
    function QueryInterface(constref Iid : TGuid; out Obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;

    function GetEnumerator: IHoldingsEnumerator;
    function GetCount: Integer;
    function AddNew: IHolding;

  public
    procedure AfterConstruction; override;
    property RefCount : longint read fRefCount;

    property ModuleName: string read GetModuleName write SetModuleName;
    property Created: TDateTime read GetCreated write SetCreated;
    property Uid: word read GetUid write SetUid;
  end;

   { THolding }

  THolding = class(TInterfacedObject, IHolding)
  private
    fStrValue: TString;
    fName: String;
    fShortDescription: String;
    fIndex:   Word;
    fVarType: TVarType;
    fValue:   DWord;
    fMultipler: DWord;
  protected
    function GetName: String;
    procedure SetName(const aName: String);
    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);
    function GetIndex: Word;
    procedure SetIndex(const aIndex: Word);
    function GetVarType: TVarType;
    procedure SetVarType(const aVarType: TVarType);
    function GetValue: DWord;
    procedure SetValue(const aValue: DWord);
    function GetValueStr: string;
    procedure SetValueStr(const aValueStr: string);
    function GetMultipler: DWord;
    procedure SetMultipler(const aMultipler: DWord);
  public
    property Name: String read GetName write SetName;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
    property ValueStr: string read GetValueStr write SetValueStr;
    property Index: Word read GetIndex write SetIndex;
    property VarType: TVarType read GetVarType write SetVarType;
    property Value: DWord read GetValue write SetValue;
    property Multipler: DWord read GetMultipler write SetMultipler;
  end;



implementation

{ THoldingsEnumerator }

function THoldingsEnumerator.QueryInterface(constref Iid: TGuid;
  out Obj): longint; stdcall;
begin
  if GetInterface(Iid, Obj) then
  Result:=S_OK
else
 Result:=longint(E_NOINTERFACE);
end;

function THoldingsEnumerator._AddRef: longint; stdcall;
begin
  _Addref:=InterlockedIncrement(fRefCount);
end;

function THoldingsEnumerator._Release: longint; stdcall;
begin
  _Release:=InterlockedDecrement(fRefCount);
  if _Release=0 then
    Self.Destroy;
end;

procedure THoldingsEnumerator.AfterConstruction;
begin
  inherited AfterConstruction;
  fRefCount := 0;
end;

 { THoldings }

function THoldings.GetCreated: TDateTime;
begin
  Result := fCreated;
end;

function THoldings.GetModuleName: string;
begin
  Result := fModuleName;
end;

function THoldings.GetUid: word;
begin
  Result := fUid;
end;

procedure THoldings.SetCreated(const aCreated: TDateTime);
begin
  fCreated := aCreated;
end;

procedure THoldings.SetModuleName(const aModuleName: string);
begin
  fModuleName := aModuleName;
end;

procedure THoldings.SetUid(const aUid: word);
begin
  fUid := aUid;
end;

function THoldings.QueryInterface(constref Iid: TGuid; out Obj): longint;
  stdcall;
begin
    if GetInterface(Iid, Obj) then
    Result:=S_OK
  else
   Result:=longint(E_NOINTERFACE);
end;

function THoldings._AddRef: longint; stdcall;
begin
  _Addref:=InterlockedIncrement(fRefCount);
end;

function THoldings._Release: longint; stdcall;
begin
  _Release:=InterlockedDecrement(fRefCount);
  if _Release=0 then
    Self.Destroy;
end;

function THoldings.GetEnumerator: IHoldingsEnumerator;
begin
  Result := THoldingsEnumerator.Create(Self) as IHoldingsEnumerator;
end;

function THoldings.GetCount: Integer;
begin
  Result := Count;
end;

function THoldings.AddNew: IHolding;
begin
  Result := THolding.Create;
  Add(Result);
end;

procedure THoldings.AfterConstruction;
begin
  inherited AfterConstruction;
  fRefCount := 0;
end;

{ THolding }

function THolding.GetMultipler: DWord;
begin
  Result := fMultipler;
end;

procedure THolding.SetMultipler(const aMultipler: DWord);
begin
  fMultipler := aMultipler;
end;

function THolding.GetName: String;
begin
  Result := fName;
end;

function THolding.GetShortDescription: String;
begin
  Result := fShortDescription;
end;

function THolding.GetValueStr: string;
begin
  Result := fStrValue;
end;

procedure THolding.SetName(const aName: String);
begin
  if not SameText(fName, aName) then
     fName := aName;
end;

procedure THolding.SetShortDescription(const aShortDescription: String);
begin
  if not SameText(fShortDescription, aShortDescription) then
     fShortDescription := aShortDescription;
end;

procedure THolding.SetValueStr(const aValueStr: string);
begin
//  if not SameText(fStrValue, aValueStr) then
     fStrValue := aValueStr;
end;

function THolding.GetIndex: Word;
begin
  Result := fIndex;
end;

function THolding.GetValue: DWord;
begin
  Result := fValue;
end;

function THolding.GetVarType: TVarType;
begin
  Result := fVarType;
end;

procedure THolding.SetIndex(const aIndex: Word);
begin
  fIndex := aIndex;
end;

procedure THolding.SetValue(const aValue: DWord);
begin
  fValue := aValue;
end;

procedure THolding.SetVarType(const aVarType: TVarType);
begin
  fVarType := aVarType;
end;
end.

