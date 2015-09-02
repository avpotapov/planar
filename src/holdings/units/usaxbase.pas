unit uSaxBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, fgl, Sax;


{$REGION Константы}

resourcestring
  // Теги и аттрибуты XML - словарь входных символов

  sHoldings = 'holdings';
  sDate     = 'date';
  sModule   = 'module';
  sUid      = 'uid';
  sHolding  = 'holding';
  sName     = 'name';
  sIndex    = 'index';
  sVarType  = 'type';
  sMultipler = 'multipler';
  sShortDescription = 'description_short';
  sValue    = 'value';
  sValueStr = 'value_str';

{$ENDREGION Константы}

type
{$REGION Типы тегов и аттрибутов}

  TTypeTagAttrName =
    (
    tnEmpty, // Помещается на вершину стека при создании
    tnHoldings,
    tnDate,
    tnModule,
    tnUid,
    tnHolding,
    tnName,
    tnIndex,
    tnVarType,
    tnMultipler,
    tnShortDescription,
    tnValue,
    tnValueStr
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


  fTagAttrMap.Add(sHoldings, tnHoldings);
  fTagAttrMap.Add(sDate, tnDate);
  fTagAttrMap.Add(sModule, tnModule);
  fTagAttrMap.Add(sUid, tnUid);
  fTagAttrMap.Add(sHolding, tnHolding);
  fTagAttrMap.Add(sName, tnName);
  fTagAttrMap.Add(sIndex, tnIndex);
  fTagAttrMap.Add(sVarType, tnVarType);
  fTagAttrMap.Add(sMultipler, tnMultipler);

  fTagAttrMap.Add(sShortDescription, tnShortDescription);
  fTagAttrMap.Add(sValue, tnValue);
  fTagAttrMap.Add(sValueStr, tnValueStr);

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
  FoundIndex := fTagAttrMap.IndexOf(aLocalName);
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
     fBuilderTransitionTable[TopTagStackType, fSavedTagAttrType].SetTagHandler(S);
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
  FoundIndex := fTagAttrMap.IndexOf(aLocalName);
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


