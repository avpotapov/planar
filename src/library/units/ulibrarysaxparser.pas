unit uLibrarySaxParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, SAX, SAX_XML,
  uLibrary, uSaxBase, uLibraryBuilder, typinfo;

type

{$REGION Factory}

  { TLibraryFactory }

  TLibraryFactory = class
  private
    fSaxReader:  TSaxReader;
    fSaxHandler: TSaxHandler;

    fLibrary: ILibrary;

  private
    procedure ConstructSaxHandler(const aFilePath: String);
    procedure ConstructSaxReader;
    procedure AssignHandler;
  public
    function GetLibrary(const aFileNames: array of String): ILibrary;
  end;

  { TSaxLibraryHandler }

  TSaxLibraryHandler = class(TSaxHandler)
  protected
    fLibrary:  ILibrary;
    fFilePath: String;
  public
    constructor Create(const aLibrary: ILibrary; const aFilePath: String); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    property Lib: ILibrary read fLibrary write fLibrary;
    property FilePath: String read fFilePath write fFilePath;
  end;

  { TModuleDefineFactory }

  TModuleDefineFactory = class
  private
    fSaxReader:  TSaxReader;
    fSaxHandler: TSaxHandler;

    fModuleDefine: IModuleDefine;
  private
    procedure ConstructSaxHandler;
    procedure ConstructSaxReader;
    procedure AssignHandler;
    function GetModuleDefine: IModuleDefine;
  public
    constructor Create(const aModuleDefine: IModuleDefine); reintroduce;
  public
    procedure ParseModule(const aFileName: String);
    property ModuleDefine: IModuleDefine read GetModuleDefine;
  end;

  { TSaxModuleDefineHandler }

  TSaxModuleDefineHandler = class(TSaxHandler)
  private
    fModuleDefine: IModuleDefine;
  public
    constructor Create(const aModuleDefine: IModuleDefine); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    property ModuleDefine: IModuleDefine read fModuleDefine;
  end;

{$ENDREGION Factory}


implementation

uses
  uLibraryCls,
  uModuleBuilder;


{$REGION LibraryFactory }
function TLibraryFactory.GetLibrary(const aFileNames: array of String): ILibrary;
var
  Stream: TStream;
  I:      Integer;
begin

  fLibrary := TLibrary.Create;

  // Последовательное открытие файлов и их парсинг
  for I := Low(aFileNames) to High(aFileNames) do
  begin
    Stream := TFileStream.Create(aFileNames[I], fmOpenRead);

    ConstructSaxHandler(ExtractFilePath(aFileNames[I]));
    ConstructSaxReader;
    AssignHandler;

    try
      try
        fSaxReader.ParseStream(Stream);
      except
        on E: ESAXParseException do
        begin
          MessageBox(0, PChar(Utf8ToAnsi(E.Message)),
            PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
            MB_ICONERROR + MB_OK);
        end;
        else
          MessageBox(0, PChar(Utf8ToAnsi('Неизвестная ошибка')),
            PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
            MB_ICONERROR + MB_OK);
      end;

    finally
      FreeAndNil(fSaxHandler);
      FreeAndNil(fSaxReader);
      FreeAndNil(Stream);
    end;

  end;
  Result := fLibrary;
end;


procedure TLibraryFactory.ConstructSaxHandler(const aFilePath: String);
begin
  fSaxHandler := TSaxLibraryHandler.Create(fLibrary, aFilePath);
end;

procedure TLibraryFactory.ConstructSaxReader;
begin
  fSaxReader := TSaxXmlReader.Create;
end;

procedure TLibraryFactory.AssignHandler;
begin
  // Назначение обработчиков парсера
  fSaxReader.OnCharacters := @fSaxHandler.OnCharacters;
  fSaxReader.OnStartElement := @fSaxHandler.OnStartElement;
  fSaxReader.OnEndElement := @fSaxHandler.OnEndElement;
  fSaxReader.OnError      := @fSaxHandler.OnError;
  fSaxReader.OnFatalError := @fSaxHandler.OnFatalError;

end;

{ TSaxLibraryHandler }

constructor TSaxLibraryHandler.Create(const aLibrary: ILibrary;
  const aFilePath: String);
begin
  inherited Create;
  fLibrary  := aLibrary;
  fFilePath := aFilePath;
end;

procedure TSaxLibraryHandler.AfterConstruction;
var
  ModuleBuilder:  IBuilder;
  LibraryBuilder: IBuilder;
begin
  {$IFDEF DEBUG}
  Assert(fIntfStack.IsEmpty);
  Assert(fTagStack.IsEmpty);
  {$ENDIF}

  inherited AfterConstruction;

  {$IFDEF DEBUG}
  Assert(fTagStack.Top = tnEmpty);
  {$ENDIF}

  // Поместить на вершину стека библиотеку
  fIntfStack.Push(fLibrary);


  // Строители
  LibraryBuilder := TLibraryBuilder.Create(Self);
  ModuleBuilder  := TModuleBuilder.Create(Self);

  // Таблица переходов
  fBuilderTransitionTable[tnEmpty, tnLib]  := LibraryBuilder;
  fBuilderTransitionTable[tnLib, tnLib]    := LibraryBuilder;
  fBuilderTransitionTable[tnLib, tnModule] := ModuleBuilder;
  fBuilderTransitionTable[tnModule, tnModule] := ModuleBuilder;
end;

procedure TSaxLibraryHandler.BeforeDestruction;
var
  Intf: IInterface;
begin
  // Удалить последний интерфейс со стека
  fIntfStack.Pop(Intf);

  {$IFDEF DEBUG}
  Assert(fTagStack.Top = tnEmpty);
  {$ENDIF}

  inherited BeforeDestruction;

  {$IFDEF DEBUG}
  Assert(fIntfStack.IsEmpty);
  Assert(fTagStack.IsEmpty);
  {$ENDIF}
end;

{$ENDREGION LibraryFactory }

{$REGION ModuleDefineFactory}
procedure TModuleDefineFactory.ConstructSaxHandler;
begin
  fSaxHandler := TSaxModuleDefineHandler.Create(fModuleDefine);
end;

procedure TModuleDefineFactory.ConstructSaxReader;
begin
  fSaxReader := TSaxXmlReader.Create;
end;

procedure TModuleDefineFactory.AssignHandler;
begin
  {$IFDEF DEBUG}
  Assert(fSaxReader <> nil);
  Assert(fSaxHandler <> nil);
  {$ENDIF}
  // Назначение обработчиков парсера
  fSaxReader.OnCharacters := @fSaxHandler.OnCharacters;
  fSaxReader.OnStartElement := @fSaxHandler.OnStartElement;
  fSaxReader.OnEndElement := @fSaxHandler.OnEndElement;
  fSaxReader.OnError      := @fSaxHandler.OnError;
  fSaxReader.OnFatalError := @fSaxHandler.OnFatalError;
end;

function TModuleDefineFactory.GetModuleDefine: IModuleDefine;
begin
  Result := fModuleDefine;
end;

constructor TModuleDefineFactory.Create(const aModuleDefine: IModuleDefine);
begin
  inherited Create;
  fModuleDefine := aModuleDefine;
end;

procedure TModuleDefineFactory.ParseModule(const aFileName: String);
var
  Stream: TStream;
begin

  Stream := TFileStream.Create(aFileName, fmOpenRead);

  ConstructSaxHandler;
  ConstructSaxReader;
  AssignHandler;

  try

    try
      fSaxReader.ParseStream(Stream);
      fModuleDefine := (fSaxHandler as TSaxModuleDefineHandler).ModuleDefine;
    except
      on E: ESAXParseException do
      begin
        MessageBox(0, PChar(Utf8ToAnsi(E.Message)),
          PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
          MB_ICONERROR + MB_OK);
      end;
      else
        MessageBox(0, PChar(Utf8ToAnsi('Неизвестная ошибка')),
          PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
          MB_ICONERROR + MB_OK);
    end;

  finally
    FreeAndNil(fSaxHandler);
    FreeAndNil(fSaxReader);
    FreeAndNil(Stream);
  end;
end;

constructor TSaxModuleDefineHandler.Create(const aModuleDefine: IModuleDefine);
begin
  inherited Create;
  fModuleDefine := aModuleDefine;
end;

procedure TSaxModuleDefineHandler.AfterConstruction;
var
  PreSetsBuilder:  IBuilder;
  PreSetsPickListBuilder: IBuilder;
  PreSetsBitsBuilder: IBuilder;
  PickItemBuilder: IBuilder;
  RegistersBuilder: IBuilder;
  VarsBuilder:     IBuilder;
  VarBuilder:      IBuilder;
  PickListBuilder: IBuilder;
  BitsBuilder:     IBuilder;
  BitBuilder:      IBuilder;
  GroupsBuilder:   IBuilder;
  GroupBuilder:    IBuilder;
begin
  {$IFDEF DEBUG}
  Assert(fIntfStack.IsEmpty);
  Assert(fTagStack.IsEmpty);
  {$ENDIF}

  inherited AfterConstruction;

  {$IFDEF DEBUG}
  Assert(fTagStack.Top = tnEmpty);
  {$ENDIF}

  fIntfStack.Push(fModuleDefine);

  // Строители
  PreSetsBuilder  := TPreSetsBuilder.Create(Self);
  PreSetsBitsBuilder := TPreSetsBitsBuilder.Create(Self);
  PreSetsPickListBuilder := TPreSetsPickListBuilder.Create(Self);
  PickItemBuilder := TPickItemBuilder.Create(Self);
  RegistersBuilder := TRegistersBuilder.Create(Self);
  VarsBuilder     := TVarsBuilder.Create(Self);
  VarBuilder      := TVarBuilder.Create(Self);
  PickListBuilder := TPickListBuilder.Create(Self);
  BitsBuilder     := TBitsBuilder.Create(Self);
  BitBuilder      := TBitBuilder.Create(Self);
  GroupsBuilder   := TGroupsBuilder.Create(Self);
  GroupBuilder    := TGroupBuilder.Create(Self);


  // Таблица переходов
  fBuilderTransitionTable[tnEmpty, tnPreSets]   := PreSetsBuilder;
  fBuilderTransitionTable[tnPreSets, tnPreSets] := PreSetsBuilder;

  fBuilderTransitionTable[tnPreSets, tnBits]  := PreSetsBitsBuilder;
  fBuilderTransitionTable[tnBits, tnBits] := PreSetsBitsBuilder;

  fBuilderTransitionTable[tnPreSets, tnPickList]  := PreSetsPickListBuilder;
  fBuilderTransitionTable[tnPickList, tnPickList] := PreSetsPickListBuilder;


  fBuilderTransitionTable[tnPickList, tnPickItem] := PickItemBuilder;
  fBuilderTransitionTable[tnPickItem, tnPickItem] := PickItemBuilder;

  fBuilderTransitionTable[tnEmpty, tnRegisters]     := RegistersBuilder;
  fBuilderTransitionTable[tnRegisters, tnRegisters] := RegistersBuilder;

  fBuilderTransitionTable[tnRegisters, tnVarSet] := VarsBuilder;
  fBuilderTransitionTable[tnVarSet, tnVarSet]    := VarsBuilder;

  fBuilderTransitionTable[tnVarSet, tnVarDefine]    := VarBuilder;
  fBuilderTransitionTable[tnVarDefine, tnVarDefine] := VarBuilder;

  fBuilderTransitionTable[tnVarDefine, tnPickList] := PickListBuilder;
  fBuilderTransitionTable[tnPickList, tnPickList]  := PickListBuilder;

  fBuilderTransitionTable[tnVarDefine, tnBits] := BitsBuilder;
  fBuilderTransitionTable[tnBits, tnBits]      := BitsBuilder;

  fBuilderTransitionTable[tnBits, tnBitDefine]      := BitBuilder;
  fBuilderTransitionTable[tnBitDefine, tnBitDefine] := BitBuilder;

  fBuilderTransitionTable[tnEmpty, tnConfiguration] := GroupsBuilder;
  fBuilderTransitionTable[tnGncSet, tnConfiguration] := GroupsBuilder;
  fBuilderTransitionTable[tnGncSet, tnGncSet] := GroupsBuilder;

  fBuilderTransitionTable[tnGncSet, tnGncVar] := GroupBuilder;
  fBuilderTransitionTable[tnGncVar, tnGncVar] := GroupBuilder;

end;

procedure TSaxModuleDefineHandler.BeforeDestruction;
var
  Intf: IInterface;
begin
  // Удалить последний интерфейс со стека
  fIntfStack.Pop(Intf);

  {$IFDEF DEBUG}
  Assert(fTagStack.Top = tnEmpty);
  {$ENDIF}

  inherited BeforeDestruction;

  {$IFDEF DEBUG}
  Assert(fIntfStack.IsEmpty);
  Assert(fTagStack.IsEmpty);
  {$ENDIF}
end;

{$ENDREGION ModuleDefineFactory}

end.
