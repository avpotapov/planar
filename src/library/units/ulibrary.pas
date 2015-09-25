unit uLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

  {$REGION ERRORS}
const
  ERROR_DUPLICATE_KEY = 1;
  ERROR_ARGUMENT_ISEMPTY = 2;
  ERROR_SAX_PARSER = 3;

resourcestring
  sERROR_DUPLICATE_KEY = 'Дубликат ключа';
  sERROR_ARGUMENT_ISEMPTY = 'Не указано значение аргумента';
  sERROR_EMPTY_TAG_NAME = 'Найден пустой тег';
  sERROR_EMPTY_ATTR_NAME = 'Найден пустой аттрибут';
  {$ENDREGION ERRORS}

  {$REGION Types, Declarations etc.}

resourcestring
  // Строковое представление типов переменных
  sUINT8L   = 'UINT8L';
  sUINT8H   = 'UINT8H';
  sUINT16   = 'UINT16';
  sSINT16   = 'SINT16';
  sUINT32   = 'UINT32';
  sSINT32   = 'SINT32';
  sUINTIP   = 'UINTIP';
  sFLOAT    = 'FLOAT';
  sBITMAP16 = 'BITMAP16';
  sBITMAP32 = 'BITMAP32';
  sBOOL     = 'BOOL';
  sBOOL_EXT = 'BOOL_EXT';
  sIO_DATA  = 'IO_DATA';
  sID_DATA  = 'ID_DATA';
  sID_VERSION = 'ID_VERSION';
  sID_SN    = 'ID_SN';
  sID_HARD  = 'ID_HARD';
  sID_SOFT  = 'ID_SOFT';
  sID_PROJECT = 'ID_PROJECT';
  sID_BOX   = 'ID_BOX';
  sID_PLANT = 'ID_PLANT';
  sID_TYPEFIRMWARE = 'ID_TYPEFIRMWARE';
  sPROC     = 'PROC';
  sUNKNOWN  = 'UNKNOWN';

  // Строковое представление уровня доступа
  sUSER      = 'Пользователь';
  sDEVELOPER = 'Разработчик';
  sSERVICE   = 'Сервис';

  // Строковое представление разновидности переменной
  sNormal = 'Обычная';
  sGauge  = 'Калибровочная';

  // Строковое представление разновидности регистров
  sHolding = 'HOLDING';
  sInput   = 'INPUT';

  // Строковое представление синхронизации: 0 Двунаправленный, 1 Принудительный, 2 Только запись
  sBedirectional = 'Двунаправленный';
  sForce    = 'Принудительный';
  sReadOnly = 'Только запись';

type

  // Разделы бибилиотеки:  разработчика, пользователя
  TTypeSublibrary = (slDeveloper, slUser);
  // Типы сигнатур:  автоопределение, RCCU, автоопределение недоступно (старые)
  TTypeSignature  = (sgNone = 0, sgAuto, sgRccu);
  // Типы bootloader'а: 1 - без записи в 120-124 смещение, 2 - с записью, 3 - с шифрованием
  TTypeBootloader = (bl1 = 1, bl2, bl3);
  // Тип регистра: Holding, Input
  TTypeRegister   = (trHolding, trInput);
  TTypeRegisters = array[TTypeRegister] of String;

  // Уровень доступа: пользователь, производитель, сервис
  TAccess   = (acUser, acDeveloper, acService);
  TAccesses = array[TAccess] of String;

  // Разновидность переменной: обычный, калибровочный
  TKind  = (kdNormal, kdGauge);
  TKinds = array[TKind] of String;

  // Режимы синхронизации: 0 Двунаправленный, 1 Принудительный, 2 Только запись
  TSynchronization  = (syBedirectional, syForce, syReadOnly);
  TSynchronizations = array[TSynchronization] of String;


  // Тип переменной
  TVarType = (
    vtUINT8L,    // младший 8-битная беззнаковая целочисленная              0 .. 255          byte
    vtUINT8H,    // старшая 8-битная беззнаковая целочисленная              0 .. 255          byte
    vtUINT16,    // 16-разрядная беззнаковая     целочисленная              0 .. 65535        word
    vtSINT16,    // 16-разрядная знаковая        целочисленная         -32768 .. -32767       smallint
    vtUINT32,    // 32-разрядная беззнаковая     целочисленная              0 .. -2147483647  dword
    vtSINT32,    // 32-разрядная знаковая        целочисленная    -2147483648 .. -2147483647  longint
    vtFLOAT,     // 32 бита с плавающей запятой                                               single
    vtBITMAP16,  // 16-разрядная беззнаковая     целочисленная              0 .. 65535        word
    vtBITMAP32,  // 32-разрядная беззнаковая     целочисленная              0 .. -2147483647  dword
    vtBOOL,      // 16-битная
    vtBOOL_EXT,  // 16-битная
    vtUINTIP,    // 32-разрядная беззнаковая     целочисленная              0 .. -2147483647  dword
    vtIO_DATA,
    vtID_DATA,
    vtID_VERSION,
    vtID_SN,
    vtID_HARD,
    vtID_SOFT,
    vtID_TYPEFIRMWARE,
    vtID_PROJECT,
    vtID_BOX,
    vtID_PLANT,
    vtPROC,
    vtUNKNOWN);

  TVarTypes = specialize TFPGMap<String, TVarType>;
  {$ENDREGION Types, Declarations etc.}

type

  {$REGION BASE}

  //////////////////////////////////////////////////////////////////////////////
  // BASE
  // Базовый интерфейс библиотеки
  IBase = interface
    ['{C55FE7E5-2B4B-4ECA-9E9B-39B823A1627D}']
    // Возвращает код ошибки, одновременно сбрасывая его в памяти
    function GetLastError: dword;
    // Возвращает описание ошибки, в т.ч. системной
    function GetErrorDesc(const aError: dword): String;
  end;


  IDescEnumerator = interface
    ['{E72BAA4A-E3F3-4C6E-88E3-332B78845ED1}']
    function GetCurrent: String;
    function MoveNext: Boolean;
    property Current: String read GetCurrent;
  end;

  // Используется для многострочного текста
  IDescription = interface(IBase)
    ['{8D02C3E0-A7D9-49D5-ADBC-6078601AA1B7}']

    function GetCount: Integer;
    function Add(const S: String): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;

    function GetTextStr: String;
    procedure SetTextStr(const Value: String);

    function Get(Index: Integer): String;

    function GetEnumerator: IDescEnumerator;

    property Text: String read GetTextStr write SetTextStr;
    property Strings[Index: Integer]: String read Get; default;
    property Count: Integer read GetCount;
  end;

  IMapEnumerator = interface
    ['{E0F17492-F0E2-4D48-A1E3-8ED9CA6BD25F}']

    function GetCurrent: Pointer;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  // Используется для переменных, пиклиста, карты битов, когда требуется
  // быстрый поиск
  generic IMap<TKey, TData> = interface(IBase)
    ['{834C7C3A-B130-4B4A-9468-B0C02AFDF179}']

    function Add(const aKey: TKey): Integer;
    function Remove(const aKey: TKey): Integer;

    function Find(const aKey: TKey; out Index: Integer): Boolean;
    function IndexOf(const aKey: TKey): Integer;
    function IndexOfData(const aData: TData): Integer;

    function ExtractData(const Item: Pointer): TData;
    function ExtractKey(const Item: Pointer): TKey;

    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(const aDuplicates: TDuplicates);

    function GetSorted: Boolean;
    procedure SetSorted(aSorted: Boolean);

    function GetKey(Index: Integer): TKey;
    function GetKeyData(const AKey: TKey): TData;
    function GetData(Index: Integer): TData;

    function GetCount: Integer;

    function GetEnumerator: IMapEnumerator;

    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Keys[Index: Integer]: TKey read GetKey;
    property Data[Index: Integer]: TData read GetData;
    property Count: Integer read GetCount;
    property Sorted: Boolean read GetSorted write SetSorted;
  end;

  generic IListEnumerator<T> = interface
    ['{3A2EEF74-2DE4-4B7E-A981-8247DF10708E}']
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  // Используется для неупорядочного списка данных
  generic IList<T> = interface(IBase)
    ['{834C7C3A-B130-4B4A-9468-B0C02AFDF179}']
    function Get(Index: Integer): T;

    function GetLast: T;
    function GetFirst: T;

    function Extract(const Item: T): T;
    function IndexOf(const Item: T): Integer;

    function Remove(const Item: T): Integer;

    function GetCount: Integer;
    procedure Insert(Index: Integer; const Item: T);
    function Add(const Item: T): Integer;
    property First: T read GetFirst;
    property Last: T read GetLast;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read Get; default;
  end;

  {$ENDREGION BASE}


  //////////////////////////////////////////////////////////////////////////////
  // LIBRARY
  {
    Интерфейсы библиотеки
  }

  ILibrary    = interface;
  ISublibrary = interface;
  IModule     = interface;
  IModuleDefine = interface;
  IBaseDescription = interface;
  IRegisters  = interface;
  IVars  = interface;
  IVarDefine = interface;
  IBits = interface;
  IBitDefine = interface;
  IPickList = interface;
  IPickItem = interface;
  IGroups = interface;
  IGroupsList = interface;
  IGroup = interface;
  IGroupItem = interface;
  IPreSets = interface;
  IPickLists = interface;
  IBitsSet = interface;
  {
    Главный класс библиотеки
  }
  ILibrarySpec = specialize IMap<TTypeSublibrary, ISublibrary>;

  ILibrary = interface(ILibrarySpec)
    ['{3080B5EE-8E6E-4113-9AE7-49FDA49CD430}']
    property SubLibrary[aTypeModule: TTypeSublibrary]: TData read GetKeyData; default;
  end;

  {
    Библиотека разработчика или пользователя, является разделами библиотеки
  }
  ISublibrarySpec = specialize IMap<Word, IModule>;

  ISublibrary = interface(ISublibrarySpec)
    ['{060A6576-A6DA-400F-B418-B0D41C5C2640}']
    property Module[aUid: Word]: TData read GetKeyData; default;
  end;

  {
    Модуль, относящийся к одному из разделов библиотеки
  }
  IModule = interface(IBase)
    ['{A050DFAF-E18A-4272-ACB8-47414201541F}']
    function GetUid: Word;
    procedure SetUid(const aUid: Word);

    function GetName: String;
    procedure SetName(const aName: String);

    function GetTypeSignature: TTypeSignature;
    procedure SetTypeSignature(const aTypeSignature: TTypeSignature);

    function GetTypeBootloader: TTypeBootloader;
    procedure SetTypeBootloader(const aTypeBootloader: TTypeBootloader);

    function GetModuleDefine: IModuleDefine;

    property Name: String read GetName write SetName;
    property Uid: Word read GetUid write SetUid;
    property TypeSignature: TTypeSignature read GetTypeSignature write SetTypeSignature;
    property TypeBootloader: TTypeBootloader
      read GetTypeBootloader write SetTypeBootloader;
    property ModuleDefine: IModuleDefine read GetModuleDefine;

  end;

  {
    Детальное описание модуля
  }
  IModuleDefine = interface(IBase)
    ['{4A44E843-C50D-419E-AD6B-55EC62CDC0B6}']

    function GetBaseDescription: IBaseDescription;
    function GetPreSets: IPreSets;
    function GetRegisters: IRegisters;
    function GetConfiguration: IGroups;

    property BaseDescription: IBaseDescription read GetBaseDescription;
    property PreSets: IPreSets read GetPreSets;
    property Registers: IRegisters read GetRegisters;
    property Configuration: IGroups read GetConfiguration;

  end;


  {
    Назначение модуля и его графическое представление
  }
  IBaseDescription = interface(IBase)
    ['{FAA522EF-E8DC-4329-9DB8-11DE590004E3}']

    function GetDescription: IDescription;

    function GetImage: String;
    procedure SetImage(const aImage: String);

    property Description: IDescription read GetDescription;
    property Image: String read GetImage write SetImage;
  end;

  {
    Предустановленные значения
  }
  IPreSets = interface(IBase)
  ['{7D6AA590-3951-46C6-91C5-AF1C42483146}']
    function GetPickLists: IPickLists;
    function GetBitsSet: IBitsSet;

    property PickLists: IPickLists read GetPickLists;
    property BitsSet: IBitsSet read GetBitsSet;
  end;

  {
     Предустановленный список значений
  }
  IPickListsSpec = specialize IMap<string, IPickList>;
  IPickLists = interface(IPickListsSpec)
   ['{1F3FA130-6B3E-444B-A1F9-96AF4AD14FCC}']
    property PickLists[aName: string]: TData read GetKeyData; default;
  end;

  {
     Предустановленный список наборов битов
  }
  IBitsSetSpec = specialize IMap<string, IBits>;
  IBitsSet = interface(IBitsSetSpec)
   ['{86EB0665-CC7B-4A61-9201-37B685FAF1B1}']
   property BitsSet[aName: string]: TData read GetKeyData; default;
  end;



  {
     Коллекция регистров
  }
  IRegistersSpec = specialize IMap<TTypeRegister, IVars>;

  IRegisters = interface(IRegistersSpec)
    ['{060A6576-A6DA-400F-B418-B0D41C5C2640}']
    property VarSet[aTypeRegister: TTypeRegister]: TData read GetKeyData; default;
    function FindByUid(const aUid: string): IVarDefine;
  end;

  {
    Коллекция переменных одного типа регистра
  }
  IVarsSpec = specialize IMap<string, IVarDefine>;
  IVars = interface(IVarsSpec)
    ['{04008726-DCE7-41BA-BF4A-25874965B827}']
    property VarDefine[aUid: string]: TData read GetKeyData; default;
  end;

  {
    Описание переменной
  }

  { IVarDefine }

  IVarDefine = interface(IBase)
    ['{6096EA10-C9C1-4238-92E5-2E379537E3D9}']

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

     procedure Copy(const aVar: IVarDefine);

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

  {
    Коллекция битов
  }

  IBitsSpec = specialize IMap<byte, IBitDefine>;
  IBits = interface(IBitsSpec)
    ['{04008726-DCE7-41BA-BF4A-25874965B827}']
    function GetName: string;
    procedure SetName(const aName: string);
    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);
    procedure Clear;
    property BitDefine[aBit: byte]: TData read GetKeyData; default;
    property Name: string read GetName write SetName;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
  end;

  {
    Описание отдельных битов
  }

  IBitDefine = interface(IBase)
    ['{2661A677-5958-48CE-ADBE-1E7957F11B56}']
    function GetIndex: byte;
    procedure SetIndex(const aIndex: byte);

    function GetName: string;
    procedure SetName(const aName: string);

    function GetShortDescription: string;
    procedure SetShortDescription(const aShortDescription: string);

    function GetDescription: IDescription;

    function GetVer: string;
    procedure SetVer(const aVer: string);

    procedure Copy(const aBit: IBitDefine);

    property Index: byte read GetIndex write SetIndex;
    property Name: string read GetName write SetName;
    property ShortDescription: string read GetShortDescription write SetShortDescription;
    property Description: IDescription read GetDescription;
    property Ver: string read GetVer write SetVer;
  end;

 {
   Список
 }
  IPickListSpec = specialize IMap<byte, IPickItem>;
  IPickList = interface(IPickListSpec)
    ['{22C21F01-1341-42AE-BEFE-2B6A219F4BD6}']
    function GetName: string;
    procedure SetName(const aName: string);

    function GetShortDescription: string;
    procedure SetShortDescription(const aShortDescription: string);
    procedure Clear;
    property PickItem[aItem: byte]: TData read GetKeyData; default;
    property Name: string read GetName write SetName;
    property ShortDescription: string read GetShortDescription write SetShortDescription;
  end;

 {
   Элемент списка
 }

 IPickItem = interface(IBase)
    ['{2661A677-5958-48CE-ADBE-1E7957F11B56}']

    function GetValue: word;
    procedure SetValue(const aValue: word);

    function GetName: string;
    procedure SetName(const aName: string);

    function GetShortDescription: string;
    procedure SetShortDescription(const aShortDescription: string);

    function GetVer: string;
    procedure SetVer(const aVer: string);

    function GetDescription: IDescription;

    procedure Copy(const aPickItem: IPickItem);

    property Value: word read GetValue write SetValue;
    property Name: string read GetName write SetName;
    property ShortDescription: string read GetShortDescription write SetShortDescription;
    property Description: IDescription read GetDescription;
    property Ver: String read GetVer write SetVer;
  end;

  {
    Коллекция групп переменных конфигурации
  }

  IGroups = interface(IBase)
    ['{4A426538-0847-433E-AA2C-C55E32CD3DB8}']

    function GetGroupsList: IGroupsList;
    function GetGroup: IGroup;
    function GetImageIndex: Integer;

    function GetShortDescription: string;
    procedure SetImageIndex(const aImageIndex: Integer);
    procedure SetShortDescription(const aShortDescription: string);

    property GroupsList: IGroupsList read GetGroupsList;
    property Group: IGroup read GetGroup;

    property ShortDescription: string read GetShortDescription write SetShortDescription;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
  end;


  {
   Список коллекций групп конфигурации
  }
  IGroupsListSpec = specialize IList<IGroups>;
  IGroupsListEnumeratorSpec = specialize IListEnumerator<IGroups>;
  IGroupsList = interface(IGroupsListSpec)
    ['{0C1A424D-6E27-413F-8C21-2CCF742D5B65}']
    function GetEnumerator: IGroupsListEnumeratorSpec;
    function AddGroups(const aShortDescription: string = ''): Integer;
    function Find(const aShortDescription: string): Integer;
  end;

  {
    Список групп
  }
  IGroupSpec = specialize IList<IGroupItem>;
  IGroupEnumeratorSpec = specialize IListEnumerator<IGroupItem>;
  IGroup = interface(IGroupSpec)
    ['{43255234-E361-4C44-A2BD-CD0D2E43B366}']
    function GetEnumerator: IGroupEnumeratorSpec;
    function AddGroupItem(const aVarDefine: IVarDefine): Integer;
  end;

  {
    Элемент группы = описание переменной и ее интерфейсное представление
  }

  IGroupItem = interface(IBase)
  ['{F18BB969-B619-44EB-AC12-E080BEF43087}']
  function GetVarDefine: IVarDefine;
  property VarDefine: IVarDefine read GetVarDefine;
end;


{$REGION EXTERNAL}
function GetLibrary(const aFileNames: array of string): ILibrary; external 'library.dll';
procedure CloseLibrary; external 'library.dll';
function GetNewGuid: String; external 'library.dll';
procedure SaveLibrary(const aLibrary: ILibrary); external 'library.dll';
{$ENDREGION EXTERNAL}



var
  VarTypes: TVarTypes;
  Accesses: TAccesses;
  Kinds:    TKinds;
  TypeRegisters: TTypeRegisters;
  Synchronizations: TSynchronizations;
  DWordTypes: set of TVarType;

implementation


procedure PopulateVarTypes();
begin
  with VarTypes do
  begin
    Add(sUINT8L, vtUINT8L);
    Add(sUINT8H, vtUINT8H);
    Add(sUINT16, vtUINT16);
    Add(sSINT16, vtSINT16);
    Add(sUINT32, vtUINT32);
    Add(sSINT32, vtSINT32);
    Add(sFLOAT, vtFLOAT);
    Add(sBITMAP16, vtBITMAP16);
    Add(sBITMAP32, vtBITMAP32);
    Add(sBOOL, vtBOOL);
    Add(sBOOL_EXT, vtBOOL_EXT);
    Add(sIO_DATA, vtIO_DATA);
    Add(sID_DATA, vtID_DATA);
    Add(sID_VERSION, vtID_VERSION);
    Add(sID_SN, vtID_SN);
    Add(sID_HARD, vtID_Hard);
    Add(sID_SOFT, vtID_SOFT);
    Add(sID_TYPEFIRMWARE, vtID_TYPEFIRMWARE);
    Add(sID_PROJECT, vtID_PROJECT);
    Add(sID_BOX, vtID_BOX);
    Add(sID_PLANT, vtID_PLANT);
    Add(sPROC, vtPROC);
    Add(sUNKNOWN, vtUNKNOWN);
  end;
end;

procedure PopulateAccesses;
begin
  Accesses[acUser]      := sUSER;
  Accesses[acDeveloper] := sDeveloper;
  Accesses[acService]   := sService;
end;

procedure PopulateKinds;
begin
  Kinds[kdNormal] := sNormal;
  Kinds[kdGauge]  := sGauge;
end;

procedure PopulateRegisters;
begin
  TypeRegisters[trHolding] := sHolding;
  TypeRegisters[trInput]   := sInput;

end;


procedure PopulateSynchronization;
begin
  Synchronizations[syBedirectional] := sBedirectional;
  Synchronizations[syForce]    := sForce;
  Synchronizations[syReadOnly] := sReadOnly;

end;


initialization

  begin
    DWordTypes := [vtUINT32, vtSINT32, vtFLOAT, vtBITMAP32, vtBOOL_EXT, vtUINTIP, vtIO_DATA];

    VarTypes := TVarTypes.Create;
    PopulateVarTypes();

    PopulateAccesses();

    PopulateKinds();

    PopulateRegisters();

    PopulateSynchronization();
  end;

finalization
  begin
    FreeAndNil(VarTypes);
  end;

end.
