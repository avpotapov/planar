unit uHoldingsBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sax,
  uSaxBase,
  uHoldings,
  uLibrary;

type

  { THoldingsBuilder }

  THoldingsBuilder = class(TBuilder)
  public
    //  В качестве аргумента - список аттрибутов узла
    procedure StartTagHandler(const aAtts: TSaxAttributes); override;
    //  В качестве аргумента - текстовое значение узла
    procedure SetTagHandler(const {%H-}aText: String); override;
    procedure EndTagHandler; override;
  end;

  { THoldingBuilder }

  THoldingBuilder = class(TBuilder)
  public
    //  В качестве аргумента - список аттрибутов узла
    procedure StartTagHandler(const aAtts: TSaxAttributes); override;
    //  В качестве аргумента - текстовое значение узла
    procedure SetTagHandler(const {%H-}aText: String); override;
    procedure EndTagHandler; override;
  end;


implementation



{ THoldingsBuilder }

procedure THoldingsBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  Holdings: IHoldings;

begin
  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty);
  {$ENDIF}

  // Проверка вершины стека
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty then
      // Получить интерфейс на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IHoldings, Holdings) then
    begin


      // Привести аттрибут тега к типу
      Holdings.Created := StrToDateTimeDef(GetAttrValue(aAtts, sDate), Now());
      Holdings.ModuleName := GetAttrValue(aAtts, sModule);
      Holdings.Uid := StrToIntDef(GetAttrValue(aAtts, sUid), 0);

      // Поместить в стек тегов новый тег
      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnHoldings);

    end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnHoldings);
  {$ENDIF}

end;

procedure THoldingsBuilder.SetTagHandler(const aText: String);
begin
{ TODO : Stub }
end;

procedure THoldingsBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Holdings: IHoldings;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnHoldings then
    // Получить интерфейс на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IHoldings, Holdings) then
    begin
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty);
  {$ENDIF}

end;


{ THoldingBuilder }

procedure THoldingBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  Holdings: IHoldings;
  Holding: IHolding;
begin

  if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnHoldings) and
    Supports(fSaxHandler.IntfStack.Top, IHoldings, Holdings) then
    begin

      Holding := Holdings.AddNew;

      // Привести аттрибут тега к типу
      Holding.Name := GetAttrValue(aAtts, sName);
      Holding.Index := StrToIntDef(GetAttrValue(aAtts, sIndex), 0);
      Holding.VarType := VarTypes[GetAttrValue(aAtts, sVarType)];
      Holding.Multipler := StrToIntDef(GetAttrValue(aAtts, sMultipler), 1);


      // Поместить в стек новый интерфейс
      fSaxHandler.IntfStack.Push(Holding);
      // Поместить в стек тегов новый тег
      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnHolding);
    end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnHolding);
  {$ENDIF}

end;

procedure THoldingBuilder.SetTagHandler(const aText: String);
var
  Holding: IHolding;
begin
  if Supports(fSaxHandler.IntfStack.Top, IHolding, Holding) then
    case  fSaxHandler.CurrentTagAttrType of
      tnShortDescription: Holding.ShortDescription := aText;
      tnValue: Holding.Value := StrToIntDef(aText, 0);
      tnValueStr: Holding.ValueStr := aText;
    end;
end;

procedure THoldingBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Holding: IHolding;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnHolding then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IHolding, Holding) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;

  {$IFDEF DEBUG}
  Assert(fSaxHandler.TagStack.Top = TTypeTagAttrName.tnHoldings);
  {$ENDIF}

end;

end.

