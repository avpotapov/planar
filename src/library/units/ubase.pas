unit uBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows,
  fgl,
  uLibrary;

type
TErrors = specialize TFpgMap<Integer, String>;




{$REGION Base}

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
{$ENDREGION Base}

{$REGION DescEnumerator}
  TDescEnumerator = class(TInterfacedObject, IDescEnumerator)
  protected
    fStrings: TStrings;
    fPosition: Integer;
    function GetCurrent: string;
  public
    constructor Create(aStrings: TStrings);
    function MoveNext: Boolean;
    property Current: string read GetCurrent;
  end;
{$ENDREGION DescEnumerator}

{$REGION TDescription}

  { TDescription }

  TDescription = class(TStringList, IDescription)
  protected
    fRefCount : longint;
    fLastError: dword;
    fErrors:    TErrors;
    fText: string;
  protected
    function GetTextStr: string; override;

    function QueryInterface(constref Iid : TGuid; out Obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
  public
    function GetEnumerator: IDescEnumerator;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    // Установить ошибку
    procedure SetLastError(const aError: dword);

    // Возвращает код ошибки, одновременно сбрасывая его в памяти
    function GetLastError: dword;
    // Возвращает описание ошибки, в т.ч. системной
    function GetErrorDesc(const aError: dword): String;

    property RefCount : longint read fRefCount;
  end;
{$ENDREGION TDescription}

{$REGION MapEnumerator}
TMapEnumerator = class(TInterfacedObject, IMapEnumerator)
  protected
    fMap: TFpsMap;
    fPosition: Integer;
    function GetCurrent: Pointer;
  public
    constructor Create(aMap: TFpsMap);
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;
{$ENDREGION MapEnumerator}

{$REGION TMap}

  { TMap }

  generic TMap<TKey, TData> = class(specialize TFpgMap<TKey, TData>, specialize IMap<TKey, TData>)
  private
    function GetSorted: boolean;

  protected
    fRefCount : longint;
    fLastError: dword;
    fErrors:    TErrors;

  protected
    function QueryInterface(constref Iid : TGuid; out Obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;

    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(const aDuplicates: TDuplicates);

    function GetCount: Integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function GetEnumerator: IMapEnumerator;

    function ExtractData(const aItem: Pointer): TData;
    function ExtractKey(const aItem: Pointer): TKey;

    // Установить ошибку
    procedure SetLastError(const aError: dword);

    // Возвращает код ошибки, одновременно сбрасывая его в памяти
    function GetLastError: dword;
    // Возвращает описание ошибки, в т.ч. системной
    function GetErrorDesc(const aError: dword): String;


    property RefCount : longint read fRefCount;

  end;
{$ENDREGION TMap}

{$REGION ListEnumerator}
generic TListEnumerator<T> = class(specialize TFpgListEnumerator<T>, specialize IListEnumerator <T>)
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
{$ENDREGION ListEnumerator}

{$REGION TList}

  { TList }

  generic TList<T> = class(specialize TFpgList<T>, specialize IList<T>)
  protected
    fRefCount : longint;
    fLastError: dword;
    fErrors:    TErrors;
  protected
    function QueryInterface(constref Iid : TGuid; out Obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;

    function GetCount: Integer;
  public
   destructor Destroy; override;

      // Установить ошибку
    procedure SetLastError(const aError: dword);

    // Возвращает код ошибки, одновременно сбрасывая его в памяти
    function GetLastError: dword;
    // Возвращает описание ошибки, в т.ч. системной
    function GetErrorDesc(const aError: dword): String;

    procedure AfterConstruction; override;
    property RefCount : longint read fRefCount;
  end;
{$ENDREGION TList}

//{ TString }
//TCharArray = array of Char;
//TString = packed record
//  CharArray: TCharArray;
//end;
//
//operator := (aString: String): TString;
//operator := (aString: TString): string;

implementation

//{ TString }
//operator :=(aString: String): TString;
//begin
//  SetLength(Result.CharArray, Length(aString) + 1);
//  System.Move(aString[1], Result.CharArray[0], Length(aString));
//end;
//
//operator :=(aString: TString): string;
//begin
//  Result := String(aString.CharArray);
//end;

{$REGION Base}

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
{$ENDREGION Base}

{$REGION DescEnumerator}

function TDescEnumerator.GetCurrent: string;
begin
  Result := fStrings[fPosition];
end;

constructor TDescEnumerator.Create(aStrings: TStrings);
begin
  inherited Create;
  fStrings := aStrings;
  fPosition := -1;
end;

function TDescEnumerator.MoveNext: Boolean;
begin
  Inc(fPosition);
  Result := fPosition < fStrings.Count;
end;
{$ENDREGION DescEnumerator}

{$REGION TDescription}

function TDescription.GetTextStr: string;
begin
  fText := inherited GetTextStr;
  Result := fText;
end;

function TDescription.QueryInterface(constref Iid: TGuid; out Obj): longint;
  stdcall;
begin
 if GetInterface(Iid, Obj) then
   Result:=S_OK
 else
  Result:=longint(E_NOINTERFACE);
end;

function TDescription._AddRef: longint; stdcall;
begin
  _Addref:=InterlockedIncrement(fRefCount);
end;

function TDescription._Release: longint; stdcall;
begin
  _Release:=InterlockedDecrement(fRefCount);
  if _Release=0 then
    Self.Destroy;
end;

function TDescription.GetEnumerator: IDescEnumerator;
begin
  Result := TDescEnumerator.Create(Self);
end;

procedure TDescription.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors := TErrors.Create;
  fRefCount := 0;
end;

destructor TDescription.Destroy;
begin
  FreeAndNil(fErrors);
  inherited Destroy;
end;

procedure TDescription.SetLastError(const aError: dword);
begin
  fLastError := aError;
end;

function TDescription.GetLastError: dword;
begin
  Result     := fLastError;
  if fLastError = 0 then
    Result := GetLastError;
  fLastError := 0;
end;

function TDescription.GetErrorDesc(const aError: dword): String;
begin
  if aError <> 0 then
    Result := fErrors.KeyData[aError]
  else
    Result := SysErrorMessage(Windows.GetLastError);
end;

{$ENDREGION TDescription}

{$REGION MapEnumerator}

constructor TMapEnumerator.Create(aMap: TFpsMap);
begin
  inherited Create;
  fMap := aMap;
  fPosition := -1;
end;

function TMapEnumerator.GetCurrent: Pointer;
begin
  Result := fMap.Items[fPosition];
end;

function TMapEnumerator.MoveNext: Boolean;
begin
  Inc(fPosition);
  Result := fPosition < fMap.Count;
end;
{$ENDREGION MapEnumerator}

{$REGION TMap}

function TMap.GetDuplicates: TDuplicates;
begin
  Result := Duplicates;
end;

procedure TMap.SetDuplicates(const aDuplicates: TDuplicates);
begin
  if Duplicates <> aDuplicates then
     Duplicates := aDuplicates;
end;

function TMap.GetSorted: boolean;
begin
  Result := Sorted;
end;

function TMap.QueryInterface(constref Iid: TGuid; out Obj): longint; stdcall;
begin
  if GetInterface(Iid, Obj) then
    Result:=S_OK
  else
   Result:=longint(E_NOINTERFACE);
end;

function TMap._AddRef: longint; stdcall;
begin
  _Addref:=InterlockedIncrement(fRefCount);
end;

function TMap._Release: longint; stdcall;
begin
  _Release:=InterlockedDecrement(fRefCount);
  if _Release=0 then
    Self.Destroy;
end;

function TMap.GetCount: Integer;
begin
  Result := Count;
end;

function TMap.GetEnumerator: IMapEnumerator;
begin
  Result := TMapEnumerator.Create(Self);
end;

function TMap.ExtractData(const aItem: Pointer): TData;
var
  PData: Pointer;
begin
  PData := PByte(aItem) + KeySize;
  Result := TData(PData^);
end;

function TMap.ExtractKey(const aItem: Pointer): TKey;
begin
  Result := TKey(aItem^);
end;

procedure TMap.SetLastError(const aError: dword);
begin
  fLastError := aError;
end;

function TMap.GetLastError: dword;
begin
  Result     := fLastError;
  if fLastError = 0 then
    Result := GetLastError;
  fLastError := 0;
end;

function TMap.GetErrorDesc(const aError: dword): String;
begin
  if aError <> 0 then
    Result := fErrors.KeyData[aError]
  else
    Result := SysErrorMessage(Windows.GetLastError);
end;

procedure TMap.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors := TErrors.Create;
  fErrors.Add(ERROR_DUPLICATE_KEY, sERROR_DUPLICATE_KEY);


  Sorted := True;
  Duplicates := TDuplicates.dupError;
  fRefCount := 0;
end;
destructor TMap.Destroy;
begin
  FreeAndNil(fErrors);
  inherited Destroy;
end;

{$ENDREGION TMap}

{$REGION ListEnumerator}

function TListEnumerator.QueryInterface(constref Iid: TGuid; out Obj): longint;
  stdcall;
begin
  if GetInterface(Iid, Obj) then
    Result:=S_OK
  else
   Result:=longint(E_NOINTERFACE);
end;

function TListEnumerator._AddRef: longint; stdcall;
begin
  _Addref:=InterlockedIncrement(fRefCount);
end;

function TListEnumerator._Release: longint; stdcall;
begin
  _Release:=InterlockedDecrement(fRefCount);
  if _Release=0 then
    Self.Destroy;
end;

procedure TListEnumerator.AfterConstruction;
begin
  inherited AfterConstruction;
  fRefCount := 0;
end;
{$ENDREGION ListEnumerator}

{$REGION TList}

function TList.QueryInterface(constref Iid: TGuid; out Obj): longint; stdcall;
begin
  if GetInterface(Iid, Obj) then
    Result:=S_OK
  else
   Result:=longint(E_NOINTERFACE);
end;

function TList._AddRef: longint; stdcall;
begin
  _Addref:=InterlockedIncrement(fRefCount);
end;

function TList._Release: longint; stdcall;
begin
  _Release:=InterlockedDecrement(fRefCount);
  if _Release=0 then
    Self.Destroy;
end;

function TList.GetCount: Integer;
begin
  Result := Count;
end;

procedure TList.SetLastError(const aError: dword);
begin
  fLastError := aError;
end;

function TList.GetLastError: dword;
begin
  Result     := fLastError;
  if fLastError = 0 then
    Result := GetLastError;
  fLastError := 0;
end;

function TList.GetErrorDesc(const aError: dword): String;
begin
  if aError <> 0 then
    Result := fErrors.KeyData[aError]
  else
    Result := SysErrorMessage(Windows.GetLastError);
end;

procedure TList.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors := TErrors.Create;
  fRefCount := 0;
end;

destructor TList.Destroy;
begin
  FreeAndNil(fErrors);
  inherited Destroy;
end;

{$ENDREGION TList}

end.

