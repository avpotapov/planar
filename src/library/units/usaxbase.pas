unit uSaxBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, fgl, Sax;

{$REGION Константы}

resourcestring
  // Теги и аттрибуты XML - словарь входных символов

  sLib      = 'lib';
  sTypeBootloader = 'type_bootloader';
  sTypeSignature = 'type_signature';
  sTypeLib  = 'type';
  sModule   = 'module';
  sBasedoc  = 'basedoc';
  sRegisters = 'registers';
  sDescription = 'description';
  sPicture  = 'picture';
  sVars     = 'var_set';
  sVarDefine = 'var_define';
  sArrayType = 'array_type';
  sBits     = 'bits';
  sPickList = 'picklist';
  sVarType  = 'type';
  sName     = 'name';
  sIndex    = 'index';
  sShortDescription = 'description_short';
  sAccess   = 'access';
  sSynchronization = 'synch_type';
  sSingleRequest = 'single_req';
  sReadAlways = 'read_always';
  sBitDefine = 'bit_define';
  sPickItem = 'picklist_item';
  sValue    = 'value';
  sChannels = 'channels';
  sChnSet   = 'chn_set';
  sChnDefine = 'chn_define';
  sChnType  = 'chn_type';
  sSubTypeSet = 'chn_sub_type_set';
  sVer      = 'ver';
  sChnName  = 'chn_name';
  sUID      = 'uid';
  sMultipler = 'multipler';
  sBit      = 'bit';
  sSubType  = 'sub_type';
  sFunctions = 'functions';
  sConfiguration = 'configuration';
  sAlarms   = 'alarms';
  sFncSet   = 'fnc_set';
  sFncVar   = 'fnc_var';
  sGncSet   = 'gnc_set';
  sVisible  = 'visible';
  sGncVar   = 'gnc_var';
  sMask     = 'mask';
  sCondition = 'condition';
  sAlarm    = 'alarm';
  sSubAlarm = 'subalarm';
  sPara     = 'para';
  sImageIndex = 'image_index';
  sKind     = 'kind';
  sTop      = 'top';
  sLeft     = 'left';
  sHeight   = 'height';
  sWidth    = 'width';
  sSize     = 'size';
  sUnit     = 'unit';
  sPreSets  = 'presets';

{$ENDREGION Константы}

type
{$REGION Типы тегов и аттрибутов}

  TTypeTagAttrName =
    (
    tnEmpty, // Помещается на вершину стека при создании
    tnLib,
    tnTypeBootloader,
    tnTypeSignature,
    tnTypeLib,
    tnModule,
    tnBasedoc,
    tnRegisters,
    tnDescription,
    tnPicture,
    tnVarSet,
    tnVarDefine,
    tnArrayType,
    tnBits,
    tnPickList,
    tnVarType,
    tnName,
    tnIndex,
    tnShortDescription,
    tnAccess,
    tnSynchronization,
    tnSingleRequest,
    tnReadAlways,
    tnBitDefine,
    tnPickItem,
    tnValue,
    tnChannels,
    tnChnSet,
    tnChnDefine,
    tnChnType,
    tnSubTypeSet,
    tnVer,
    tnChnName,
    tnUID,
    tnMultipler,
    tnBit,
    tnSubType,
    tnFunctions,
    tnConfiguration,
    tnAlarms,
    tnFncSet,
    tnFncVar,
    tnGncSet,
    tnVisible,
    tnGncVar,
    tnMask,
    tnCondition,
    tnAlarm,
    tnSubAlarm,
    tnPara,
    tnImageIndex,
    tnKind,
    tnTop,
    tnLeft,
    tnHeight,
    tnWidth,
    tnSize,
    tnUnit,
    tnPreSets
    );
  {$ENDREGION Типы тегов и аттрибутов}


{$REGION FpgStack}

 generic TFpgStack<T> = class(specialize TFpgList<T>)
 public
    procedure Push(const aItem: T);
    procedure Pop(out aItem: T);
    function Top: T;
    function IsEmpty: boolean;
 end;
{$ENDREGION FpgStack}

{$REGION Builder}

  IBuilder = interface
    ['{4F526BB1-2E44-4FD5-8646-B5A91376960A}']
    procedure StartTagHandler(const aAtts: TSaxAttributes);
    procedure SetTagHandler(const aText: String);
    procedure EndTagHandler;
  end;

  TSaxHandler = class;

  { TBuilder }

  TBuilder = class(TInterfacedObject, IBuilder)
  protected
    fSaxHandler: TSaxHandler;
  protected
    // Получение значения аттрибута
    function GetAttrValue(const aAtts: TSaxAttributes; const aAttrName: String): String;
  public
    constructor Create(const aSaxHandler: TSaxHandler); reintroduce;
  public
    procedure StartTagHandler(const aAtts: TSaxAttributes); virtual; abstract;
    procedure SetTagHandler(const aText: String); virtual; abstract;
    procedure EndTagHandler; virtual; abstract;
  end;

{$ENDREGION Builder}


{$REGION SaxHandler}
  // Отображение название тега в тип
  TTagAttrMap = specialize TFpgMap<String, TTypeTagAttrName>;

  // Таблица переходов
  // На основе тега на вершине стека и нового найденного тега вызывается
  // соответствующий строитель

  // Стек типов тегов
  TTagStack = specialize TFpgStack<TTypeTagAttrName>;
  TIntfStack = specialize TFpgStack<IInterface>;

  TBuilderTransitionTable = array [TTypeTagAttrName, TTypeTagAttrName] of IBuilder;
  { TSaxHandler }

  TSaxHandler = class
  protected
    fCurrentTagAttrType: TTypeTagAttrName;
    fTagAttrMap: TTagAttrMap;
    fTagStack:   TTagStack;
    fIntfStack: TIntfStack;
    fSavedTagAttrType: TTypeTagAttrName;
    fBuilderTransitionTable: TBuilderTransitionTable;
  public
    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
  public

    procedure OnStartElement(Sender: TObject;
      const {%H-}aNamespaceURI, aLocalName, {%H-}aQName: SAXString;
    {%H-}aAtts: TSAXAttributes); virtual;

    procedure OnEndElement(Sender: TObject; const {%H-}aNamespaceURI,
    {%H-}aLocalName, {%H-}aQName: SAXString); virtual;

    procedure OnCharacters(Sender: TObject; const aCh: PSAXChar;
    {%H-}aStart, aLength: Integer); virtual;

    procedure OnError(Sender: TObject; aException: ESAXParseException); virtual;

    procedure OnFatalError(Sender: TObject; aException: ESAXParseException); virtual;

  public
    property TagStack: TTagStack read fTagStack;
    property IntfStack: TIntfStack read fIntfStack;
    property CurrentTagAttrType: TTypeTagAttrName read fCurrentTagAttrType
      write fCurrentTagAttrType;
  end;

{$ENDREGION SaxHandler}

implementation

{$REGION TFpgStack}

procedure TFpgStack.Push(const aItem: T);
begin
  Add(aItem);
end;

procedure TFpgStack.Pop(out aItem: T);
begin
  if Count > 0 then
  begin
    aItem := Last;
    Remove(Last);
  end;
end;

function TFpgStack.Top: T;
begin
  if Count > 0 then
    Result := Last;
end;

function TFpgStack.IsEmpty: boolean;
begin
  Result := Count = 0;
end;
{$ENDREGION TFpgStack}

{$REGION Builder}
constructor TBuilder.Create(const aSaxHandler: TSaxHandler);
begin
  inherited Create;
  fSaxHandler := aSaxHandler;
end;

function TBuilder.GetAttrValue(const aAtts: TSaxAttributes;
  const aAttrName: String): String;
begin
  Result := '';
  if aAtts <> nil then
    Result := aAtts.GetValue('', aAttrName);
end;

{$ENDREGION Builder}

{$REGION SaxHandler}

constructor TSaxHandler.Create;
begin
  inherited Create;
  fTagAttrMap := TTagAttrMap.Create;
  fTagStack   := TTagStack.Create;
  fIntfStack := TIntfStack.Create;
end;

procedure TSaxHandler.AfterConstruction;
begin
  inherited AfterConstruction;

  fTagAttrMap.Add(sLib, tnLib);
  fTagAttrMap.Add(sTypeBootloader, tnTypeBootloader);
  fTagAttrMap.Add(sTypeSignature, tnTypeSignature);
  fTagAttrMap.Add(sTypeLib, tnTypeLib);
  fTagAttrMap.Add(sModule, tnModule);
  fTagAttrMap.Add(sBasedoc, tnBasedoc);
  fTagAttrMap.Add(sRegisters, tnRegisters);
  fTagAttrMap.Add(sDescription, tnDescription);
  fTagAttrMap.Add(sPicture, tnPicture);
  fTagAttrMap.Add(sVars, tnVarSet);
  fTagAttrMap.Add(sVarDefine, tnVarDefine);
  fTagAttrMap.Add(sArrayType, tnArrayType);
  fTagAttrMap.Add(sBits, tnBits);
  fTagAttrMap.Add(sPickList, tnPickList);
  fTagAttrMap.Add(sVarType, tnVarType);
  fTagAttrMap.Add(sName, tnName);
  fTagAttrMap.Add(sIndex, tnIndex);
  fTagAttrMap.Add(sShortDescription, tnShortDescription);
  fTagAttrMap.Add(sAccess, tnAccess);
  fTagAttrMap.Add(sSynchronization, tnSynchronization);
  fTagAttrMap.Add(sSingleRequest, tnSingleRequest);
  fTagAttrMap.Add(sReadAlways, tnReadAlways);
  fTagAttrMap.Add(sBitDefine, tnBitDefine);
  fTagAttrMap.Add(sPickItem, tnPickItem);
  fTagAttrMap.Add(sValue, tnValue);
  fTagAttrMap.Add(sChannels, tnChannels);
  fTagAttrMap.Add(sChnSet, tnChnSet);
  fTagAttrMap.Add(sChnDefine, tnChnDefine);
  fTagAttrMap.Add(sChnType, tnChnType);
  fTagAttrMap.Add(sSubTypeSet, tnSubTypeSet);
  fTagAttrMap.Add(sVer, tnVer);
  fTagAttrMap.Add(sChnName, tnChnName);
  fTagAttrMap.Add(sUID, tnUID);
  fTagAttrMap.Add(sMultipler, tnMultipler);
  fTagAttrMap.Add(sBit, tnBit);
  fTagAttrMap.Add(sSubType, tnSubType);
  fTagAttrMap.Add(sFunctions, tnFunctions);
  fTagAttrMap.Add(sConfiguration, tnConfiguration);
  fTagAttrMap.Add(sAlarms, tnAlarms);
  fTagAttrMap.Add(sFncSet, tnFncSet);
  fTagAttrMap.Add(sFncVar, tnFncVar);
  fTagAttrMap.Add(sGncSet, tnGncSet);
  fTagAttrMap.Add(sVisible, tnVisible);
  fTagAttrMap.Add(sGncVar, tnGncVar);
  fTagAttrMap.Add(sMask, tnMask);
  fTagAttrMap.Add(sCondition, tnCondition);
  fTagAttrMap.Add(sAlarm, tnAlarm);
  fTagAttrMap.Add(sSubAlarm, tnSubAlarm);
  fTagAttrMap.Add(sPara, tnPara);
  fTagAttrMap.Add(sImageIndex, tnImageIndex);
  fTagAttrMap.Add(sKind, tnKind);
  fTagAttrMap.Add(sTop, tnTop);
  fTagAttrMap.Add(sLeft, tnLeft);
  fTagAttrMap.Add(sHeight, tnHeight);
  fTagAttrMap.Add(sWidth, tnWidth);
  fTagAttrMap.Add(sSize, tnSize);
  fTagAttrMap.Add(sUnit, tnUnit);
  fTagAttrMap.Add(sPreSets, tnPreSets);

  fTagStack.Push(tnEmpty);

end;

procedure TSaxHandler.BeforeDestruction;
var
  TagAttrType: TTypeTagAttrName;
begin
  inherited BeforeDestruction;
  fTagStack.Pop(TagAttrType);
end;

destructor TSaxHandler.Destroy;
begin
  FreeAndNil(fTagAttrMap);
  FreeAndNil(fTagStack);
  FreeAndNil(fIntfStack);
  inherited Destroy;
end;

procedure TSaxHandler.OnStartElement(Sender: TObject;
  const aNamespaceURI, aLocalName, aQName: SAXString; aAtts: TSAXAttributes);
var
  TopTagStackType: TTypeTagAttrName;
  FoundIndex: Integer;
begin
  // Если встречаем начало любого тега, определяем его тип.
  // На основе типа тега в стеке тегов и текущего типа тега по таблице
  // переходов определяем соответствующий строитель и запускаем его
  FoundIndex := fTagAttrMap.IndexOf(AnsiToUtf8(aLocalName));
  if FoundIndex < 0 then
    Exit;
  fCurrentTagAttrType := fTagAttrMap.Data[FoundIndex];
  TopTagStackType := fTagStack.Top;
  if fBuilderTransitionTable[TopTagStackType, fCurrentTagAttrType] <> nil then
  begin
     fBuilderTransitionTable[TopTagStackType, fCurrentTagAttrType].StartTagHandler(aAtts);
     // Сохранить для OnCharacters
     // Позволяет выбрать нужный строитель для внесения в объекты информации из
     // текстовых узлов
     fSavedTagAttrType := fCurrentTagAttrType;
  end;

end;

procedure TSaxHandler.OnCharacters(Sender: TObject; const aCh: PSAXChar;
  aStart, aLength: Integer);
var
  S: SaxString;
  TopTagStackType: TTypeTagAttrName;
begin
  TopTagStackType := fTagStack.Top;
  //   Сохранение массива символов Ansi в строку Utf8
  SetLength(S, aLength);
  Move(aCh^, S[1], aLength * SizeOf(SaxChar));

  if fBuilderTransitionTable[TopTagStackType, fSavedTagAttrType] <> nil then
     fBuilderTransitionTable[TopTagStackType, fSavedTagAttrType].SetTagHandler((S));
end;

procedure TSaxHandler.OnEndElement(Sender: TObject;
  const aNamespaceURI, aLocalName, aQName: SAXString);
var
  TagAttrType: TTypeTagAttrName;
  TopTagStackType: TTypeTagAttrName;
  FoundIndex: Integer;
begin
  // Если встречаем начало любого тега, определяем его тип.
  // На основе типа тега в стеке тегов и текущего типа тега по таблице
  // переходов определяем соответствующий строитель и запускаем его
  FoundIndex := fTagAttrMap.IndexOf(AnsiToUtf8(aLocalName));
  if FoundIndex < 0 then
    Exit;
  TagAttrType := fTagAttrMap.Data[FoundIndex];

  TopTagStackType := fTagStack.Top;
  if fBuilderTransitionTable[TopTagStackType, TagAttrType] <> nil then
     fBuilderTransitionTable[TopTagStackType, TagAttrType].EndTagHandler;
end;

procedure TSaxHandler.OnError(Sender: TObject; aException: ESAXParseException);
begin
  MessageBox(0, PChar(aException.Message),
    PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
    MB_ICONERROR + MB_OK);
end;

procedure TSaxHandler.OnFatalError(Sender: TObject; aException: ESAXParseException);
begin
  MessageBox(0, PChar(aException.Message),
    PChar(Utf8ToAnsi('Фатальная ошибка парсинга XML-файла')),
    MB_ICONSTOP + MB_OK);
end;

{$ENDREGION SaxHandler}
end.

