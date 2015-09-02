unit uLibraryCls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLibrary, uBase;

type

  TLibrary   = class;
  TSublibrary = class;
  TModule    = class;
  TModuleDefine = class;
  TBaseDescription = class;
  TRegisters = class;
  TVars      = class;
  TVarDefine = class;
  TBits      = class;
  TBitDefine = class;
  TPickList  = class;
  TPickItem  = class;
  TGroups   = class;
  TGroup = class;
  TGroupItem = class;
  TPreSets = class;
  TPickLists = class;
  TBitsSet = class;



  {$REGION Library}

  TLibrarySpec = specialize TMap<TTypeSublibrary, ISublibrary>;

  TLibrary = class(TLibrarySpec, ILibrary)
  public
    function Add(const AKey: TKey): Integer; reintroduce;
  end;

  {$ENDREGION Library}

  {$REGION Sublibrary}

  TSublibrarySpec = specialize TMap<Word, IModule>;

  TSublibrary = class(TSublibrarySpec, ISublibrary)
  public
    function Add(const AKey: TKey): Integer; reintroduce;
  end;

  {$ENDREGION Sublibrary}

  {$REGION Module}

  TModule = class(TBase, IModule)
  private
    fSublibrary: TSublibrary;
    fTypeSignature: TTypeSignature;
    fTypeBootloader: TTypeBootloader;
    fName: String;
    fModuleDefine: IModuleDefine;
  protected
    function GetUid: Word;
    procedure SetUid(const aUid: Word);

    function GetName: String;
    procedure SetName(const aName: String);
    function GetTypeBootloader: TTypeBootloader;
    procedure SetTypeBootloader(const aTypeBootloader: TTypeBootloader);

    function GetTypeSignature: TTypeSignature;
    procedure SetTypeSignature(const aTypeSignature: TTypeSignature);
    function GetModuleDefine: IModuleDefine;
  public
    constructor Create(const aSublibrary: TSublibrary); reintroduce;
    procedure AfterConstruction; override;

  public
    property Name: String read GetName write SetName;
    property Uid: Word read GetUid write SetUid;
    property TypeSignature: TTypeSignature read GetTypeSignature write SetTypeSignature;
    property TypeBootloader: TTypeBootloader
      read GetTypeBootloader write SetTypeBootloader;
    property ModuleDefine: IModuleDefine read GetModuleDefine;
  end;

  {$ENDREGION Module}

  {$REGION ModuleDefine}

  { TModuleDefine }

  TModuleDefine = class(TBase, IModuleDefine)
  private
    fBaseDescription: IBaseDescription;
    fPreSets: IPreSets;
    fRegisters: IRegisters;
    fGroups:   IGroups;
  protected
    function GetBaseDescription: IBaseDescription;
    function GetPreSets: IPreSets;
    function GetRegisters: IRegisters;
    function GetConfiguration: IGroups;
  public
    property BaseDescription: IBaseDescription read GetBaseDescription;
    property PreSets: IPreSets read GetPreSets;
    property Registers: IRegisters read GetRegisters;
    property Configuration: IGroups read GetConfiguration;
  end;

  {$ENDREGION ModuleDefine}

  {$REGION BaseDescription}

  TBaseDescription = class(TBase, IBaseDescription)
  private
    fDescription: IDescription;
    fImage: String;
  protected
    function GetDescription: IDescription;
    function GetImage: String;
    procedure SetImage(const aImage: String);
  public
    property Description: IDescription read GetDescription;
    property Image: String read GetImage write SetImage;
  end;

  {$ENDREGION BaseDescription}

  {$REGION PreSets}

   { TPreSets }

   TPreSets = class(TBase, IPreSets)
   private
     fPickLists: IPickLists;
     fBitsSet: IBitsSet;
   protected
     function GetPickLists: IPickLists;
     function GetBitsSet: IBitsSet;
   public
      property BitsSet: IBitsSet read GetBitsSet;
   end;

  {$ENDREGION PreSets}


  {$REGION PickLists}
  TPickListsSpec = specialize TMap<string, IPickList>;

  { TPickLists }

  TPickLists = class(TPickListsSpec, IPickLists)
  public
    function Add(const AKey: TKey): Integer; reintroduce;
  end;

  {$ENDREGION PickLists}

  {$REGION BitsSet}
  TBitsSetSpec = specialize TMap<string, IBits>;

  { TBitsSet }

  TBitsSet = class(TBitsSetSpec, IBitsSet)
  public
    function Add(const AKey: TKey): Integer; reintroduce;
  end;

  {$ENDREGION BitsSet}


  {$REGION Registers}
  TRegistersSpec = specialize TMap<TTypeRegister, IVars>;

  { TRegisters }

  TRegisters = class(TRegistersSpec, IRegisters)
  public
    function Add(const AKey: TKey): Integer; reintroduce;
    function FindByUid(const aUid: string): IVarDefine;
  end;

  {$ENDREGION Registers}

  {$REGION Vars}
  TVarsSpec = specialize TMap<String, IVarDefine>;

  TVars = class(TVarsSpec, IVars)
  private
    fTypeRegister: TTypeRegister;
  public
    constructor Create(aTypeRegister: TTypeRegister); reintroduce;
  public
    function Add(const AKey: TKey): Integer; reintroduce;
  end;

  {$ENDREGION Vars}

  {$REGION VarDefine}


  { TVarDefine }

  TVarDefine = class(TBase, IVarDefine)
    // Ссылка на контейнер регистров
    fVars: TVars;

    // Имя регистра
    fName: String;

    // Краткое описание
    fShortDescription: string;//TString;
    // Полное описание переменной
    fDescription: IDescription;

    // Версия переменной
    fVer: String;

    // Уровень доступа: uGeneral
    fAccess: TAccess;

    // Тип переменной: uGeneral
    fVarType: TVarType;

    // Разновидность переменной: uGeneral
    fKind: TKind;

    // Читать всегда
    fReadAlways: Boolean;

    // Множитель
    fMultipler: Dword;

    // Индекс
    fIndex: Word;

    // Только единичный запрос регистра
    fSingleRequest: Boolean;

    // Режим синхронизации
    fSynchronization: TSynchronization;

    // Коллекция битов
    fBits: IBits;

    // Список значений
    fPickList: IPickList;

    // Единица измерения
    fMeasure: string;

  private
    function GetAccess: TAccess;
    procedure SetAccess(const aAccess: TAccess);

    function GetVarType: TVarType;
    procedure SetVarType(const aVarType: TVarType);

    function GetKind: TKind;
    procedure SetKind(const aKind: TKind);

    function GetReadAlways: Boolean;
    procedure SetReadAlways(const aReadAlways: Boolean);

    function GetDescription: IDescription;

    function GetMultipler: DWord;
    procedure SetMultipler(const aMultipler: DWord);

    function GetIndex: Word;
    procedure SetIndex(const aIndex: Word);

    function GetName: String;
    procedure SetName(const aName: String);

    function GetTypeRegister: TTypeRegister;

    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);


    function GetSingleRequest: Boolean;
    procedure SetSingleRequest(const aSingleRequest: Boolean);

    function GetUid: String;

    function GetVer: String;
    procedure SetVer(const aVer: String);


    function GetSynchronization: TSynchronization;
    procedure SetSynchronization(const aSynchronization: TSynchronization);

    procedure SetMeasure(const aMeasure: string);
    function GetMeasure: string;


    function GetBits: IBits;
    procedure SetBits(const aBits: IBits);
    function GetPickList: IPickList;
    procedure SetPickList(const aPickList: IPickList);
  public
    constructor Create(const aVars: TVars); reintroduce;
    procedure Copy(const aVar: IVarDefine);

  public
    property Access: TAccess read GetAccess write SetAccess;
    property Description: IDescription read GetDescription;
    property Index: Word read GetIndex write SetIndex;
    property Kind: TKind read GetKind write SetKind;
    property Multipler: DWord read GetMultipler write SetMultipler;
    property Name: String read GetName write SetName;
    property TypeRegister: TTypeRegister read GetTypeRegister;
    property ReadAlways: Boolean read GetReadAlways write SetReadAlways;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
    property SingleRequest: Boolean read GetSingleRequest write SetSingleRequest;
    property Synchronization: TSynchronization
      read GetSynchronization write SetSynchronization;
    property Uid: String read GetUid;
    property VarType: TVarType read GetVarType write SetVarType;
    property Ver: String read GetVer write SetVer;
    property Bits: IBits read GetBits write SetBits;
    property Picklist: IPickList read GetPickList write SetPickList;
    property Measure: string read GetMeasure write SetMeasure;
  end;

  {$ENDREGION VarDefine}

  {$REGION Bits}
  TBitsSpec = specialize TMap<Byte, IBitDefine>;

  TBits = class(TBitsSpec, IBits)
  private
    fBitsSet: TBitsSet;
    fShortDescription: string;
  protected
    function GetName: string;
    procedure SetName(const aName: string);

    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);
  public
    constructor Create(const aBitsSet: TBitsSet); overload;

  public
    function Add(const AKey: TKey): Integer; reintroduce;
    property Name: string read GetName write SetName;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
  end;

  {$ENDREGION Bits}

  {$REGION BitDefine}

  { TBitDefine }

  TBitDefine = class(TBase, IBitDefine)
  private
    fBits: TBits;
    fName: String;
    fShortDescription: String;
    fDescription: IDescription;
    fVer:  String;

  private
    function GetIndex: Byte;
    procedure SetIndex(const aIndex: Byte);

    function GetName: String;
    procedure SetName(const aName: String);

    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);

    function GetDescription: IDescription;

    function GetVer: String;
    procedure SetVer(const aVer: String);

  public
    constructor Create(const aBits: TBits); reintroduce;
    procedure Copy(const aBit: IBitDefine);
  public
    property Index: Byte read GetIndex write SetIndex;
    property Name: String read GetName write SetName;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
    property Description: IDescription read GetDescription;
    property Ver: String read GetVer write SetVer;
  end;

  {$ENDREGION BitDefine}

  {$REGION PickList}
  TPickListSpec = specialize TMap<Byte, IPickItem>;

  TPickList = class(TPickListSpec, IPickList)
  private
    fPickLists: TPickLists;
    fName: string;
    fShortDescription: string;
  protected
    function GetName: string;
    procedure SetName(const aName: string);

    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);
  public
    constructor Create(const aPickLists: TPickLists); overload;

  public
    function Add(const AKey: TKey): Integer; reintroduce;
    property Name: string read GetName write SetName;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
  end;

  {$ENDREGION PickList}

  {$REGION PickItem}

  { TPickItem }

  TPickItem = class(TBase, IPickItem)
  private
    fPickList: TPickList;
    fName:     String;
    fShortDescription: String;
    fDescription: IDescription;
    fVer: string;
  private
    function GetValue: Word;
    procedure SetValue(const aValue: Word);

    function GetName: String;
    procedure SetName(const aName: String);

    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);

    function GetDescription: IDescription;

    function GetVer: String;
    procedure SetVer(const aVer: String);

  public
    constructor Create(const aPickList: TPickList); reintroduce;
    procedure Copy(const aPickItem: IPickItem);
  public
    property Value: Word read GetValue write SetValue;
    property Name: String read GetName write SetName;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
    property Description: IDescription read GetDescription;
    property Ver: String read GetVer write SetVer;
  end;

  {$ENDREGION PickItem}

  {$REGION Groups}

  { TGroups }

  TGroups = class(TBase, IGroups)
  private
    fShortDescription: String;
    fGroupsList: IGroupsList;
    fGroup: IGroup;

  protected
    function GetGroupsList: IGroupsList;
    function GetGroup: IGroup;

    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);

  public
    constructor Create(const aShortDescription: String = ''); reintroduce;

  public
    property GroupsList: IGroupsList read GetGroupsList;
    property Group: IGroup read GetGroup;

    property ShortDescription: String read GetShortDescription write SetShortDescription;
  end;

  {$ENDREGION Groups}

   {$REGION GroupsList}

  TGroupsListSpec = specialize TList<IGroups>;

  { TConfigsList }

  TGroupsList = class(TGroupsListSpec, IGroupsList)
  type
    TListEnumeratorSpec = specialize TListEnumerator<IGroups>;
  public
    procedure AfterConstruction; override;
  public
    function GetEnumerator: IGroupsListEnumeratorSpec;
    function AddGroups(const aShortDescription: String = ''): Integer;
    function Find(const aShortDescription: string): Integer;
  end;

  {$ENDREGION GroupsList}

  {$REGION Group}

  TGroupSpec = specialize TList<IGroupItem>;

  TGroup = class(TGroupSpec, IGroup)
    type
    TListEnumeratorSpec = specialize TListEnumerator<IGroupItem>;
  public
    function GetEnumerator: IGroupEnumeratorSpec;
    function AddGroupItem(const aVarDefine: IVarDefine): Integer;
  end;

  {$ENDREGION Group}

 {$REGION GroupItem}

  TGroupItem = class(TBase, IGroupItem)
  private
    fVarDefine: IVarDefine;

  protected
    function GetVarDefine: IVarDefine;

  public
    constructor Create(const aVarDefine: IVarDefine); reintroduce;
    property VarDefine: IVarDefine read GetVarDefine;
  end;

  {$ENDREGION GroupItem}

implementation


{$REGION Library }

function TLibrary.Add(const AKey: TKey): Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TLibrarySpec).Add(AKey, TSublibrary.Create as ISublibrary);
end;

{$ENDREGION Library}

{$REGION Sublibrary}

function TSublibrary.Add(const AKey: TKey): Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TSublibrarySpec).Add(AKey, TModule.Create(Self) as IModule);

end;

{$ENDREGION Sublibrary}

{$REGION Module}

constructor TModule.Create(const aSublibrary: TSublibrary);
begin
  inherited Create;
  fSublibrary := aSublibrary;
end;

procedure TModule.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors.Add(ERROR_DUPLICATE_KEY, sERROR_DUPLICATE_KEY);
end;

function TModule.GetUid: Word;
begin
  // Ключ по индексу объекта
  Result := fSublibrary.Keys[fSublibrary.IndexOfData(Self)];
end;

procedure TModule.SetUid(const aUid: Word);
var
  Index: Integer;
begin
  if Uid = aUid then
    Exit;

  // Для изменения ключа отключается сортировка
  fSublibrary.Sorted := False;
  try
    // Если введенный Uid уже присутствует в списке
    // генерируется исключение
    if fSublibrary.Find(aUid, Index) then
    begin
      fLastError := ERROR_DUPLICATE_KEY;
      Exit;
    end;

    // Записывается новое значение ключа
    fSublibrary.Keys[fSublibrary.IndexOfData(Self)] := aUid;
  finally
    fSublibrary.Sorted := True;
  end;
end;

function TModule.GetName: String;
begin
  Result := fName;
end;

procedure TModule.SetName(const aName: String);
begin
  if not SameText(fName, aName) then
    fName := aName;
end;

function TModule.GetTypeBootloader: TTypeBootloader;
begin
  Result := fTypeBootloader;
end;

procedure TModule.SetTypeBootloader(const aTypeBootloader: TTypeBootloader);
begin
  if fTypeBootloader <> aTypeBootloader then
    fTypeBootloader := aTypeBootloader;
end;

function TModule.GetTypeSignature: TTypeSignature;
begin
  Result := fTypeSignature;
end;

procedure TModule.SetTypeSignature(const aTypeSignature: TTypeSignature);
begin
  if fTypeSignature <> aTypeSignature then
    fTypeSignature := aTypeSignature;
end;

function TModule.GetModuleDefine: IModuleDefine;
begin
  if fModuleDefine = nil then
    fModuleDefine := TModuleDefine.Create;
  Result := fModuleDefine;
end;

{$ENDREGION Module}

{$REGION ModuleDefine}

function TModuleDefine.GetBaseDescription: IBaseDescription;
begin
  if fBaseDescription = nil then
    fBaseDescription := TBaseDescription.Create;
  Result := fBaseDescription;
end;

function TModuleDefine.GetPreSets: IPreSets;
begin
  if fPreSets = nil then
    fPreSets := TPreSets.Create;
  Result := fPreSets;
end;

function TModuleDefine.GetRegisters: IRegisters;
begin
  if fRegisters = nil then
    fRegisters := TRegisters.Create;
  Result := fRegisters;
end;

function TModuleDefine.GetConfiguration: IGroups;
begin
  if fGroups = nil then
    fGroups := TGroups.Create;
  Result     := fGroups;
end;

{$ENDREGION ModuleDefine}

{$REGION BaseDescription}

function TBaseDescription.GetDescription: IDescription;
begin
  if fDescription = nil then
    fDescription := TDescription.Create;
  Result := fDescription;
end;

function TBaseDescription.GetImage: String;
begin
  Result := fImage;
end;

procedure TBaseDescription.SetImage(const aImage: String);
begin
  if not SameText(fImage, aImage) then
    fImage := aImage;
end;

{$ENDREGION BaseDescription}


{$REGION PreSets}

function TPreSets.GetPickLists: IPickLists;
begin
  if fPickLists = nil then
    fPickLists := TPickLists.Create;
  Result := fPickLists;
end;

function TPreSets.GetBitsSet : IBitsSet;
begin
  if fBitsSet = nil then
    fBitsSet := TBitsSet.Create;
  Result := fBitsSet;
end;

{$ENDREGION PreSets}

{$ENDREGION PickLists}

function TPickLists.Add(const AKey: TKey): Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TPickListsSpec).Add(AKey, TPickList.Create(Self) as IPickList);

end;

{$ENDREGION PickLists}


{$REGION BitsSet}

function TBitsSet.Add(const AKey : TKey) : Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TBitsSetSpec).Add(AKey, TBits.Create(Self) as IBits);

end;
{$ENDREGION BitsSet}

{$REGION Registers}

function TRegisters.Add(const AKey: TKey): Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TRegistersSpec).Add(AKey, TVars.Create(AKey) as IVars);
end;

function TRegisters.FindByUid(const aUid: string): IVarDefine;
var
  Index: Integer;
  Vars: IVars;
  P: Pointer;
begin
  Result := nil;

  for P in Self do
  begin
    Vars := ExtractData(P);
    Index := Vars.IndexOf(aUid);
    if Index >= 0 then
    begin
      Result := Vars.Data[Index];
      Break;
    end;
  end;

end;

{$ENDREGION Registers}

{$REGION Vars}

constructor TVars.Create(aTypeRegister: TTypeRegister);
begin
  inherited Create;
  fTypeRegister := aTypeRegister;
end;

function TVars.Add(const AKey: TKey): Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TVarsSpec).Add(AKey, TVarDefine.Create(Self) as IVarDefine);
end;

{$ENDREGION Vars}

{$REGION VarDefine}

constructor TVarDefine.Create(const aVars: TVars);
begin
  inherited Create;
  fVars := aVars;
end;

procedure TVarDefine.Copy(const aVar: IVarDefine);
var
  P: Pointer;
  Bit: IBitDefine;
  PickItem: IPickItem;
begin
  fName := aVar.Name;
  fAccess := aVar.Access;
  fKind := aVar.Kind;
  fMeasure := aVar.Measure;
  fMultipler := aVar.Multipler;
  fReadAlways := aVar.ReadAlways;
  fShortDescription := aVar.ShortDescription;
  fSingleRequest := aVar.SingleRequest;
  fSynchronization := aVar.Synchronization;
  fVarType := aVar.VarType;
  fVer := aVar.Ver;
  Description.Text := aVar.Description.Text;

  if aVar.Bits.Count > 0 then
  	for P in aVar.Bits do
    begin
    	Bit := aVar.Bits.ExtractData(P);
      fBits.Add(Bit.Index);
      fBits[Bit.Index].Copy(Bit);
    end;

  if aVar.PickList.Count > 0 then
  	for P in aVar.PickList do
    begin
    	PickItem := aVar.PickList.ExtractData(P);
      fPickList.Add(PickItem.Value);
      fPickList[PickItem.Value].Copy(PickItem);
    end;
end;

function TVarDefine.GetAccess: TAccess;
begin
  Result := fAccess;
end;

function TVarDefine.GetMeasure: string;
begin
  Result := fMeasure;
end;

procedure TVarDefine.SetAccess(const aAccess: TAccess);
begin
  if fAccess <> aAccess then
    fAccess := aAccess;
end;

function TVarDefine.GetVarType: TVarType;
begin
  Result := fVarType;
end;

procedure TVarDefine.SetMeasure(const aMeasure: string);
begin
//  if not SameText(fMeasure, aMeasure) then
    fMeasure := aMeasure;
end;


procedure TVarDefine.SetVarType(const aVarType: TVarType);
begin
  if fVarType <> aVarType then
    fVarType := aVarType;
end;

function TVarDefine.GetKind: TKind;
begin
  Result := fKind;
end;

procedure TVarDefine.SetKind(const aKind: TKind);
begin
  if fKind <> aKind then
    fKind := aKind;
end;

function TVarDefine.GetReadAlways: Boolean;
begin
  Result := fReadAlways;
end;

procedure TVarDefine.SetReadAlways(const aReadAlways: Boolean);
begin
  if fReadAlways <> aReadAlways then
    fReadAlways := aReadAlways;
end;

function TVarDefine.GetDescription: IDescription;
begin
  if fDescription = nil then
    fDescription := TDescription.Create;
  Result := fDescription;
end;

function TVarDefine.GetMultipler: DWord;
begin
  Result := fMultipler;
end;

procedure TVarDefine.SetMultipler(const aMultipler: DWord);
begin
  if fMultipler <> aMultipler then
    fMultipler := aMultipler;
end;

function TVarDefine.GetIndex: Word;
begin
  Result := fIndex;
end;

procedure TVarDefine.SetIndex(const aIndex: Word);
begin
  if fIndex <> aIndex then
    fIndex := aIndex;
end;

function TVarDefine.GetName: String;
begin
  Result := fName;
end;

procedure TVarDefine.SetName(const aName: String);
begin
  if not SameText(fName, aName) then
    fName := aName;
end;

function TVarDefine.GetTypeRegister: TTypeRegister;
begin
  Result := fVars.fTypeRegister;
end;

function TVarDefine.GetShortDescription: String;
begin
  Result := fShortDescription;
end;

procedure TVarDefine.SetShortDescription(const aShortDescription: String);
begin
  if not SameText(fShortDescription, aShortDescription) then
    fShortDescription := aShortDescription;
end;

function TVarDefine.GetSingleRequest: Boolean;
begin
  Result := fSingleRequest;
end;

procedure TVarDefine.SetSingleRequest(const aSingleRequest: Boolean);
begin
  if fSingleRequest <> aSingleRequest then
    fSingleRequest := aSingleRequest;
end;

function TVarDefine.GetUid: String;
var
  I: Integer;
begin
  I      := fVars.IndexOfData(Self);
  Result := fVars.Keys[I];
end;

function TVarDefine.GetVer: String;
begin
  Result := fVer;
end;

procedure TVarDefine.SetVer(const aVer: String);
begin
  if not SameText(fVer, aVer) then
    fVer := aVer;
end;

function TVarDefine.GetSynchronization: TSynchronization;
begin
  Result := fSynchronization;
end;

procedure TVarDefine.SetSynchronization(const aSynchronization: TSynchronization);
begin
  if fSynchronization <> aSynchronization then
    fSynchronization := aSynchronization;
end;

function TVarDefine.GetBits: IBits;
begin
  if fBits = nil then
    fBits := TBits.Create;
  Result  := fBits;
end;

procedure TVarDefine.SetBits(const aBits : IBits);
begin
  fBits := aBits;
end;

function TVarDefine.GetPickList: IPickList;
begin
  if fPickList = nil then
    fPickList := TPickList.Create;
  Result      := fPickList;
end;

procedure TVarDefine.SetPickList(const aPickList: IPickList);
begin
  fPickList := aPickList;
end;

{$ENDREGION VarDefine}

{$REGION Bits}

function TBits.GetName : string;
var
  FoundIndex: Integer;
begin
  Result := '';
  if fBitsSet = nil then
    Exit;
  FoundIndex := fBitsSet.IndexOfData(Self as IBits);
  if FoundIndex < 0 then
    Exit;
  Result := fBitsSet.Keys[FoundIndex];
end;

procedure TBits.SetName(const aName : string);
var
  I: Integer;
begin
  if fBitsSet = nil then
    Exit;

  if SameText(GetName,  aName) then
    Exit;

  // Для изменения ключа отключается сортировка
  fBitsSet.Sorted := False;
  try
    // В случае дубликата возбуждается исключение
    if fBitsSet.Find(aName, I) then
    begin
      fLastError := ERROR_DUPLICATE_KEY;
      Exit;
    end;
    // Записывается новое значение ключа
    fBitsSet.Keys[fBitsSet.IndexOfData(Self)] := aName;
  finally
    fBitsSet.Sorted := True;
  end;

end;

function TBits.GetShortDescription: String;
begin
  Result := fShortDescription;
end;

procedure TBits.SetShortDescription(const aShortDescription: String);
begin
  if not SameText(fShortDescription, aShortDescription) then
     fShortDescription := aShortDescription;
end;

constructor TBits.Create(const aBitsSet : TBitsSet);
begin
  inherited Create;
  fBitsSet := aBitsSet;
end;


function TBits.Add(const AKey: TKey): Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TBitsSpec).Add(AKey, TBitDefine.Create(Self) as IBitDefine);

end;

{$ENDREGION Bits}
{$REGION BitDefine}

constructor TBitDefine.Create(const aBits: TBits);
begin
  inherited Create;
  fBits := aBits;
end;

procedure TBitDefine.Copy(const aBit: IBitDefine);
begin
  fName := aBit.Name;
  fVer := aBit.Ver;
  fShortDescription := aBit.ShortDescription;
  Description.Text := aBit.Description.Text;
end;

function TBitDefine.GetIndex: Byte;
var
  I: Integer;
begin
  I      := fBits.IndexOfData(Self);
  Result := fBits.Keys[I];
end;

procedure TBitDefine.SetIndex(const aIndex: Byte);
var
  I: Integer;
begin
  if Index = aIndex then
    Exit;

  // Для изменения ключа отключается сортировка
  fBits.Sorted := False;
  try
    // В случае дубликата возбуждается исключение
    if fBits.Find(aIndex, I) then
    begin
      fLastError := ERROR_DUPLICATE_KEY;
      Exit;
    end;
    // Записывается новое значение ключа
    fBits.Keys[fBits.IndexOfData(Self)] := aIndex;
  finally
    fBits.Sorted := True;
  end;

end;

function TBitDefine.GetName: String;
begin
  Result := fName;
end;

procedure TBitDefine.SetName(const aName: String);
begin
  if not SameText(fName, aName) then
    fName := aName;
end;

function TBitDefine.GetShortDescription: String;
begin
  Result := fShortDescription;
end;

procedure TBitDefine.SetShortDescription(const aShortDescription: String);
begin
  if not SameText(fShortDescription, aShortDescription) then
    fShortDescription := aShortDescription;
end;

function TBitDefine.GetDescription: IDescription;
begin
  if fDescription = nil then
    fDescription := TDescription.Create;
  Result := fDescription;
end;

function TBitDefine.GetVer: String;
begin
  Result := fVer;
end;

procedure TBitDefine.SetVer(const aVer: String);
begin
  if not SameText(fVer, aVer) then
    fVer := aVer;
end;

{$ENDREGION BitDefine}

{$REGION PickList}

function TPickList.GetName: string;
var
  FoundIndex: Integer;
begin
  Result := fName;
  if fPickLists = nil then
    Exit;
  FoundIndex := fPickLists.IndexOfData(Self as IPickList);
  if FoundIndex < 0 then
    Exit;
  Result := fPickLists.Keys[FoundIndex];
end;

procedure TPickList.SetName(const aName: string);
var
  I: Integer;
begin
  if fPickLists = nil then
    Exit;

  if SameText(GetName,  aName) then
    Exit;

  // Для изменения ключа отключается сортировка
  fPickLists.Sorted := False;
  try
    // В случае дубликата возбуждается исключение
    if fPickLists.Find(aName, I) then
    begin
      fLastError := ERROR_DUPLICATE_KEY;
      Exit;
    end;
    // Записывается новое значение ключа
    fPickLists.Keys[fPickLists.IndexOfData(Self)] := aName;
    fName := aName;
  finally
    fPickLists.Sorted := True;
  end;
end;

function TPickList.GetShortDescription: String;
begin
	Result := fShortDescription;
end;

procedure TPickList.SetShortDescription(const aShortDescription: String);
begin
	fShortDescription := aShortDescription;
end;

constructor TPickList.Create(const aPickLists: TPickLists);
begin
  inherited Create;
  fPicklists := aPicklists;
end;


function TPickList.Add(const AKey: TKey): Integer;
var
  FoundIndex: Integer;
begin
  if Find(AKey, FoundIndex) then
  begin
    fLastError := ERROR_DUPLICATE_KEY;
    Result     := FoundIndex;
    Exit;
  end;
  Result := (Self as TPickListSpec).Add(AKey, TPickItem.Create(Self) as IPickItem);

end;

{$ENDREGION PickList}

{$REGION PickItem}

constructor TPickItem.Create(const aPickList: TPickList);
begin
  inherited Create;
  fPickList := aPickList;
end;

procedure TPickItem.Copy(const aPickItem: IPickItem);
begin
  fName := aPickItem.Name;
  fVer := aPickItem.Ver;
  fShortDescription := aPickItem.ShortDescription;
  Description.Text := aPickItem.Description.Text;
end;

function TPickItem.GetValue: Word;
var
  I: Integer;
begin
  I      := fPickList.IndexOfData(Self);
  Result := fPickList.Keys[I];

end;

procedure TPickItem.SetValue(const aValue: Word);
var
  I: Integer;
begin
  if Value = aValue then
    Exit;

  // Для изменения ключа отключается сортировка
  fPickList.Sorted := False;
  try
    // В случае дубликата возбуждается исключение
    if fPickList.Find(aValue, I) then
    begin
      fLastError := ERROR_DUPLICATE_KEY;
      Exit;
    end;
    // Записывается новое значение ключа
    fPickList.Keys[fPickList.IndexOfData(Self)] := aValue;
  finally
    fPickList.Sorted := True;
  end;
end;

function TPickItem.GetName: String;
begin
  Result := fName;
end;

procedure TPickItem.SetName(const aName: String);
begin
  if not SameText(fName, aName) then
    fName := aName;
end;

function TPickItem.GetShortDescription: String;
begin
  Result := fShortDescription;
end;

procedure TPickItem.SetShortDescription(const aShortDescription: String);
begin
  if not SameText(fShortDescription, aShortDescription) then
    fShortDescription := aShortDescription;
end;

function TPickItem.GetDescription: IDescription;
begin
  if fDescription = nil then
    fDescription := TDescription.Create;
  Result := fDescription;
end;

function TPickItem.GetVer: String;
begin
  Result := fVer;
end;

procedure TPickItem.SetVer(const aVer: String);
begin
  if not SameText(fVer, aVer) then
    fVer := aVer;
end;

{$ENDREGION PickItem}

{$REGION Groups}

function TGroups.GetGroupsList: IGroupsList;
begin
  if fGroupsList = nil then
    fGroupsList := TGroupsList.Create as IGroupsList;
  Result := fGroupsList;
end;

function TGroups.GetGroup: IGroup;
begin
  if fGroup = nil then
    fGroup := TGroup.Create as IGroup;
  Result := fGroup;
end;

function TGroups.GetShortDescription: String;
begin
  Result := fShortDescription;
end;

procedure TGroups.SetShortDescription(const aShortDescription: String);
begin
  if not SameText(fShortDescription, aShortDescription) then
    fShortDescription := aShortDescription;
end;

constructor TGroups.Create(const aShortDescription: String);
begin
  inherited Create;
  fShortDescription := aShortDescription;
end;

{$ENDREGION Groups}

{$REGION GroupsList }

procedure TGroupsList.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrors.Add(ERROR_ARGUMENT_ISEMPTY, sERROR_ARGUMENT_ISEMPTY);
end;

function TGroupsList.GetEnumerator: IGroupsListEnumeratorSpec;
begin
  Result := TListEnumeratorSpec.Create(Self) as IGroupsListEnumeratorSpec;
end;

function TGroupsList.AddGroups(const aShortDescription: String): Integer;
begin
  Result := Add(TGroups.Create(aShortDescription));
end;

function TGroupsList.Find(const aShortDescription: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  if aShortDescription = '' then
  begin
    fLastError := ERROR_ARGUMENT_ISEMPTY;
    Exit;
  end;
  for I := 0 to Count - 1 do
    if SameText(Items[I].ShortDescription, aShortDescription) then
    begin
      Result := I;
      Exit;
    end;

end;

{$ENDREGION GroupsList }

{$REGION Group}

function TGroup.GetEnumerator: IGroupEnumeratorSpec;
begin
  Result := TListEnumeratorSpec.Create(Self) as IGroupEnumeratorSpec;
end;

function TGroup.AddGroupItem(const aVarDefine: IVarDefine): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].VarDefine = aVarDefine then
    begin
      fLastError := ERROR_DUPLICATE_KEY;
      Result     := I;
      Exit;
    end;
  Result := Add(TGroupItem.Create(aVarDefine));
end;

{$ENDREGION Group}

{$REGION GroupItem}

function TGroupItem.GetVarDefine: IVarDefine;
begin
  Result := fVarDefine;
end;

constructor TGroupItem.Create(const aVarDefine: IVarDefine);
begin
  inherited Create;
  fVarDefine := aVarDefine;
end;

{$ENDREGION GroupItem}

end.
