unit uLibraryBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sax,
  uSaxBase,
  uLibrary;

type
  {$REGION LibraryBuilders}

  { TLibraryBuilder }

  TLibraryBuilder = class(TBuilder)
  public
    //  В качестве аргумента - список аттрибутов узла
    procedure StartTagHandler(const aAtts: TSaxAttributes); override;
    //  В качестве аргумента - текстовое значение узла
    procedure SetTagHandler(const {%H-}aText: String); override;
    procedure EndTagHandler; override;
  end;

  { TModuleBuilder }

  TModuleBuilder = class(TBuilder)
  public
    procedure StartTagHandler(const aAtts: TSaxAttributes); override;
    procedure SetTagHandler(const aText: string); override;
    procedure EndTagHandler; override;
  end;

{$ENDREGION LibraryBuilders}

implementation
uses
   uLibrarySaxParser;

{$REGION LibraryBuilders}

{ TLibraryBuilder }

procedure TLibraryBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  TypeSublibrary: TTypeSublibrary;
  Sublibrary: ISublibrary;
  Lib: ILibrary;
begin
  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty);
  {$ENDIF}

  // Проверка вершины стека
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, ILibrary, Lib) then
    begin

      // Привести аттрибут тега к типу
      if GetAttrValue(aAtts, sTypeLib) = 'developer' then
        TypeSublibrary := TTypeSublibrary.slDeveloper
      else
        TypeSublibrary := TTypeSublibrary.slUser;

      // Создать библиотеку
      Lib.Add(TypeSublibrary);
      if Lib.GetLastError = 0 then
      begin
        SubLibrary := TSaxLibraryHandler(fSaxHandler).Lib[TypeSublibrary];
        fSaxHandler.IntfStack.Push(SubLibrary);
      end;

      // Поместить в стек тегов новый тег
      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnLib);
  end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnLib);
  {$ENDIF}
end;

procedure TLibraryBuilder.SetTagHandler(const aText: String);
begin
  {$IFDEF DEBUG}
  Assert(True);
  {$ENDIF}
end;

procedure TLibraryBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Sublibrary: ISublibrary;
  Intf: IInterface;
begin
  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnLib);
  {$ENDIF}

  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnLib then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, ISublibrary, Sublibrary) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty);
  {$ENDIF}
end;

{ TModuleBuilder }

procedure TModuleBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  Module: IModule;
  Sublibrary: ISublibrary;
  UId: word;
  TypeBootloader: TTypeBootLoader;
  TypeSignature: TTypeSignature;
begin

  if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnLib) and
    Supports(fSaxHandler.IntfStack.Top, ISublibrary, SubLibrary) then
    begin

      // Получение значений аттрибутов
      Uid := StrToIntDef(GetAttrValue(aAtts, sUid), 0);

      TypeBootloader := TTypeBootLoader(
        StrToIntDef(GetAttrValue(aAtts, sTypeBootloader), 0));

      TypeSignature := TTypeSignature(
        StrToIntDef(GetAttrValue(aAtts, sTypeSignature), 0));

      // Попытка добавить новый модуль
      Sublibrary.Add(Uid);
      if Sublibrary.GetLastError = 0 then
      begin
        Module := Sublibrary[Uid];
        Module.TypeBootloader := TypeBootloader;
        Module.TypeSignature  := TypeSignature;

        // Поместить в стек новый интерфейс
        fSaxHandler.IntfStack.Push(Module);
        // Поместить в стек тегов новый тег
        fSaxHandler.TagStack.Push(TTypeTagAttrName.tnModule);
      end;
    end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnModule);
  {$ENDIF}
end;

procedure TModuleBuilder.SetTagHandler(const aText: string);
const
  FILE_EXT = '.jlb';
var
  S: string;
  Module: IModule;
begin
  if Supports(fSaxHandler.IntfStack.Top, IModule, Module) then
  begin
    // Название модуля
    Module.Name:= aText;
    S := (fSaxHandler as TSaxLibraryHandler).FilePath + aText + FILE_EXT;
    with TModuleDefineFactory.Create(Module.ModuleDefine) do
    try
      ParseModule(S);
    finally
      Free;
    end;
  end;
end;

procedure TModuleBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Module: IModule;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnModule then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IModule, Module) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnLib);
  {$ENDIF}
end;

{$ENDREGION LibraryBuilders}
end.

