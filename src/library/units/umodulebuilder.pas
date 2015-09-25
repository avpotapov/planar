unit uModuleBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sax,
  uSaxBase,
  uLibrary;

type
  {$REGION ModuleDefineBuilders}

    { TPresetsBuilder }

    TPresetsBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const {%H-}aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;


    { TPreSetsBitsBuilder }

    TPreSetsBitsBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;


    { TPreSetsPickListBuilder }

    TPreSetsPickListBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;

    { TPickItemBuilder }

    TPickItemBuilder = class(TBuilder)
    private
      fName: String;
      fShortDescription: String;
      fVer: String;
    private
      procedure SetDefault;
    public
      procedure StartTagHandler(const {%H-}aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const aText: string); override;
      procedure EndTagHandler; override;
    end;

    { TRegistersBuilder }

    TRegistersBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const {%H-}aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;

    { TVarsBuilder }

    TVarsBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;


    { TVarBuilder }

    TVarBuilder = class(TBuilder)
    private

      fVer:     string;
      fName:    string;
      fIndex:   word;
      fShortDescription: string;
      fAccess:  TAccess;
      fSynchronization: TSynchronization;
      fSingleRequest: boolean;
      fReadAlways: boolean;
      fMultipler: DWord;
      fVarType: TVarType;
      fKind:    TKind;
      fUnit: string;

    private
      procedure SetDefault;
    public
      procedure StartTagHandler(const aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const aText: string); override;
      procedure EndTagHandler; override;
    end;

    { TPickListBuilder }

    TPickListBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;


    { TBitsBuilder }

    TBitsBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const {%H-}aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;

    { TBitBuilder }

    TBitBuilder = class(TBuilder)
    private
      fName: string;
      fShortDescription: string;
      fVer:  string;
   private
      procedure SetDefault;
    public
      procedure StartTagHandler(const aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const aText: string); override;
      procedure EndTagHandler; override;
    end;

    { TGroupsBuilder }

    TGroupsBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const {%H-}aText: string); override;
      procedure EndTagHandler; override;
    end;

    { TGroupBuilder }

    TGroupBuilder = class(TBuilder)
    public
      procedure StartTagHandler(const {%H-}aAtts: TSaxAttributes); override;
      procedure SetTagHandler(const aText: string); override;
      procedure EndTagHandler; override;
    end;

  {$ENDREGION ModuleDefineBuilders}

implementation

uses
  uLibrarySaxParser;

{$REGION ModuleDefineBuilders}

{ TPresetsBuilder }

procedure TPresetsBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  ModuleDefine: IModuleDefine;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IModuleDefine, ModuleDefine) then
    begin
      // Поместить в стек новый интерфейс
      fSaxHandler.IntfStack.Push(ModuleDefine.PreSets);
      // Поместить в стек тегов новый тег
      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnPreSets);

    end;
end;

procedure TPresetsBuilder.SetTagHandler(const aText: string);
begin
{ TODO : Stub }
end;

procedure TPresetsBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  PreSets: IPreSets;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnPreSets then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IPreSets, Presets) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;


end;

{ TPreSetsBitsBuilder }

procedure TPreSetsBitsBuilder.StartTagHandler(const aAtts : TSaxAttributes);
var
  Bits: IBits;
  PreSets: IPresets;
  Name: string;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnPreSets then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IPresets, Presets) then
    begin
      // Получение значений аттрибутов
      Name := GetAttrValue(aAtts, sName);
      if Name = '' then
        Exit;
      PreSets.BitsSet.Add(Name);
      if PreSets.BitsSet.GetLastError = 0 then
      begin
        Bits := PreSets.BitsSet[Name];
        // Поместить в стек новый интерфейс
        fSaxHandler.IntfStack.Push(Bits);
        // Поместить в стек тегов новый тег
        fSaxHandler.TagStack.Push(TTypeTagAttrName.tnBits);

      end;
    end;

end;

procedure TPreSetsBitsBuilder.SetTagHandler(const aText : string);
var
  Bits: IBits;
begin
  case fSaxHandler.CurrentTagAttrType of
    tnShortDescription:
      if Supports(fSaxHandler.IntfStack.Top, IBits, Bits) then
        Bits.ShortDescription := aText;
  end;
end;

procedure TPreSetsBitsBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Bits: IBits;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnBits then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IBits, Bits) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;
end;



{ TPreSetsPickListBuilder }

procedure TPreSetsPickListBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  PickList: IPickList;
  PreSets: IPresets;
  Name: string;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnPreSets then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IPresets, Presets) then
    begin
      // Получение значений аттрибутов
      Name := GetAttrValue(aAtts, sName);
      if Name = '' then
        Exit;
      PreSets.PickLists.Add(Name);
      if PreSets.PickLists.GetLastError = 0 then
      begin
        PickList := PreSets.PickLists[Name];
        // Поместить в стек новый интерфейс
        fSaxHandler.IntfStack.Push(PickList);
        // Поместить в стек тегов новый тег
        fSaxHandler.TagStack.Push(TTypeTagAttrName.tnPickList);

      end;
    end;
end;

procedure TPreSetsPickListBuilder.SetTagHandler(const aText: string);
var
  PickList: IPickList;
begin

  case fSaxHandler.CurrentTagAttrType of
    tnShortDescription:
    	if Supports(fSaxHandler.IntfStack.Top, IPickList, PickList) then
    		PickList.ShortDescription := aText;
  end;
end;

procedure TPreSetsPickListBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  PickList: IPickList;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnPickList then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IPickList, PickList) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;

end;

{ TPickItemBuilder }

procedure TPickItemBuilder.SetDefault;
begin
  fName := '';
  fVer := '';
  fShortDescription := '';
end;

procedure TPickItemBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  PickList: IPickList;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnPickList then
  begin
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IPickList, PickList) then
    begin
      SetDefault;
      fVer := GetAttrValue(aAtts, sVer);
    end;
  end;
end;

procedure TPickItemBuilder.SetTagHandler(const aText: string);
var
  PickList: IPickList;
  PickItem: IPickItem;
  Value: Word;
begin
  case fSaxHandler.CurrentTagAttrType of

    tnName: fName := aText;

    tnShortDescription: fShortDescription := aText;

    tnValue:
      begin
        Value := StrToIntDef(aText, 0);
        if Supports(fSaxHandler.IntfStack.Top, IPickList, PickList) then
        begin
          PickList.Add(Value);
          if PickList.GetLastError = 0 then
          begin
            PickItem := PickList[Value];
            fSaxHandler.IntfStack.Push(PickItem);
            // Поместить в стек тегов новый тег
            fSaxHandler.TagStack.Push(TTypeTagAttrName.tnPickItem);
          end;
        end;
      end;

    tnPara:
      begin
        if Supports(fSaxHandler.IntfStack.Top, IPickItem, PickItem) then
          PickItem.Description.Add(aText);
      end;

  end;
end;

procedure TPickItemBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  PickItem: IPickItem;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnPickItem then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IPickItem, PickItem) then
    begin
      PickItem.Name := fName;
      PickItem.ShortDescription := fShortDescription;
      PickItem.Ver:= fVer;

      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
    end;



end;

{ TRegistersBuilder }

procedure TRegistersBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  ModuleDefine: IModuleDefine;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IModuleDefine, ModuleDefine) then
    begin
      // Поместить в стек новый интерфейс
      fSaxHandler.IntfStack.Push(ModuleDefine.Registers);
      // Поместить в стек тегов новый тег
      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnRegisters);

    end;

end;

procedure TRegistersBuilder.SetTagHandler(const aText: string);
begin
{ TODO : Stub }
end;

procedure TRegistersBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Registers: IRegisters;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnRegisters then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IRegisters, Registers) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;

end;

{ TVarsBuilder }

procedure TVarsBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  Registers: IRegisters;
  Vars: IVars;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnRegisters then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IRegisters, Registers) then
    begin

      // Создаем объект
      if GetAttrValue(aAtts, sArrayType) = sHolding then
      begin
        Registers.Add(trHolding);
        if Registers.GetLastError = 0 then
          Vars := Registers[trHolding];
      end;

      if GetAttrValue(aAtts, sArrayType) = sInput then
      begin
        Registers.Add(trInput);
        if Registers.GetLastError = 0 then
          Vars := Registers[trInput];
      end;

      // Поместить в стек новый интерфейс
      if Vars <> nil then
      begin
        fSaxHandler.IntfStack.Push(Vars);
        // Поместить в стек тегов новый тег
        fSaxHandler.TagStack.Push(TTypeTagAttrName.tnVarSet);
      end;

    end;

end;

procedure TVarsBuilder.SetTagHandler(const aText: string);
begin
{ TODO : Stub }
end;

procedure TVarsBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Vars: IVars;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnVarSet then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IVars, Vars) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);

     end;

end;

{ TVarBuilder }

procedure TVarBuilder.SetDefault;
begin
  fVer := '';
  fVarType := TVarType.vtUNKNOWN;
  fName := '';
  fIndex := 0;
  fShortDescription := '';
  fAccess := TAccess(0);
  fSynchronization := TSynchronization(0);
  fSingleRequest := False;
  fReadAlways := False;
  fMultipler := 1;
  fKind := TKind(0);
  fUnit := '';
end;

procedure TVarBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  Vars: IVars;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnVarSet then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IVars, Vars) then
    begin
      // Инициализация полей по умолчанию
      SetDefault;

      fVer := GetAttrValue(aAtts, sVer);
      fVarType := VarTypes[GetAttrValue(aAtts, sVarType)];
    end;
end;

procedure TVarBuilder.SetTagHandler(const aText: string);
var
  Vars: IVars;
  VarDefine: IVarDefine;
begin
  case fSaxHandler.CurrentTagAttrType of

      tnName:
      begin
        fName := aText;
      end;

      tnIndex:
      begin
        fIndex := StrToIntDef(aText, 0);
      end;

      tnShortDescription:
      begin
        fShortDescription := aText;
      end;

      tnAccess:
      begin
        fAccess := TAccess(StrToIntDef(aText, 0));
      end;

      tnSynchronization:
      begin
        fSynchronization := TSynchronization(StrToIntDef(aText, 0));
      end;

      tnSingleRequest:
      begin
        fSingleRequest := StrToIntDef(aText, 0) = 1;
      end;

      tnReadAlways:
      begin
        fReadAlways := StrToIntDef(aText, 0) = 1;
      end;

      tnMultipler:
      begin
        fMultipler := StrToIntDef(aText, 0);
      end;

      tnKind:
      begin
        fKind := TKind(StrToIntDef(aText, 0));
      end;

      tnUID:
      begin
        if Supports(fSaxHandler.IntfStack.Top, IVars, Vars) then
        begin
          Vars.Add(aText);
         if Vars.GetLastError = 0 then
          begin
            VarDefine := Vars[aText];
            fSaxHandler.IntfStack.Push(VarDefine);
            // Поместить в стек тегов новый тег
            fSaxHandler.TagStack.Push(TTypeTagAttrName.tnVarDefine);

          end;
        end;
      end;

      tnPara:
      begin
        if Supports(fSaxHandler.IntfStack.Top, IVarDefine, VarDefine) then
          VarDefine.Description.Add(aText);
      end;

      tnUnit:
      begin
        fUnit := aText;
      end;
    end;
end;

procedure TVarBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  VarDefine: IVarDefine;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnVarDefine then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IVarDefine, VarDefine) then
    begin

      VarDefine.Name    := fName;
      VarDefine.Ver     := fVer;
      VarDefine.ShortDescription := fShortDescription;
      VarDefine.Access  := fAccess;
      VarDefine.Index   := fIndex;
      VarDefine.Kind    := fKind;
      VarDefine.Multipler := fMultipler;
      VarDefine.SingleRequest := fSingleRequest;
      VarDefine.ReadAlways := fReadAlways;
      VarDefine.VarType := fVarType;
      VarDefine.Synchronization := fSynchronization;
      VarDefine.Measure:= fUnit;

      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);


    end;
end;

{ TPickListBuilder }

procedure TPickListBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  VarDefine: IVarDefine;
  Name: string;
  FoundIndex: Integer;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnVarDefine then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IVarDefine, VarDefine) then
    begin
      // Получение значений аттрибутов
      Name := GetAttrValue(aAtts, sName);
      if Name <> '' then
      begin
        FoundIndex := TSaxModuleDefineHandler(fSaxHandler).ModuleDefine.PreSets.PickLists.IndexOf(Name);
        if FoundIndex = -1 then
          Exit;
        VarDefine.Picklist := TSaxModuleDefineHandler(fSaxHandler).ModuleDefine.PreSets.PickLists.Data[FoundIndex];
        Exit;
      end;

      // Поместить в стек новый интерфейс
      fSaxHandler.IntfStack.Push(VarDefine.Picklist);
      // Поместить в стек тегов новый тег
      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnPickList);
    end;
end;

procedure TPickListBuilder.SetTagHandler(const aText: string);
var
  PickList: IPickList;
begin

  case fSaxHandler.CurrentTagAttrType of
    tnShortDescription:
    	if Supports(fSaxHandler.IntfStack.Top, IPickList, PickList) then
    		PickList.ShortDescription := aText;
  end;
end;

procedure TPickListBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  PickList: IPickList;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnPickList then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IPickList, PickList) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;
end;

{ TBitsBuilder }

procedure TBitsBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  VarDefine: IVarDefine;
  Name: string;
  FoundIndex: Integer;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnVarDefine then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IVarDefine, VarDefine) then
    begin
      // Получение значений аттрибутов
      Name := GetAttrValue(aAtts, sName);
      if Name <> '' then
      begin
        FoundIndex := TSaxModuleDefineHandler(fSaxHandler).ModuleDefine.PreSets.BitsSet.IndexOf(Name);
        if FoundIndex = -1 then
          Exit;
        VarDefine.Bits := TSaxModuleDefineHandler(fSaxHandler).ModuleDefine.PreSets.BitsSet.Data[FoundIndex];
        VarDefine.Bits.Name := Name;
        Exit;
      end;

      // Поместить в стек новый интерфейс
      fSaxHandler.IntfStack.Push(VarDefine.Bits);
      // Поместить в стек тегов новый тег
      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnBits);
    end;

(*
var
  VarDefine: IVarDefine;
  Bits: IBits;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnVarDefine then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IVarDefine, VarDefine) then
    begin
      // Поместить в стек новый интерфейс
      Bits := VarDefine.Bits;
      fSaxHandler.IntfStack.Push(Bits);
      // Поместить в стек тегов новый тег

      fSaxHandler.TagStack.Push(TTypeTagAttrName.tnBits);
    end;
*)
end;

procedure TBitsBuilder.SetTagHandler(const aText: string);
var
  Bits: IBits;
begin
  case fSaxHandler.CurrentTagAttrType of
    tnShortDescription:
    begin
      if Supports(fSaxHandler.IntfStack.Top, IBits, Bits) then
        Bits.ShortDescription := aText;
    end;
  end;
end;

procedure TBitsBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Bits: IBits;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnBits then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IBits, Bits) then
    begin
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);
      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
    end;
end;


{ TBitBuilder }

procedure TBitBuilder.SetDefault;
begin
  fName := '';
  fShortDescription := '';
  fVer := '';
end;

procedure TBitBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  Bits: IBits;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnBits then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IBits, Bits) then
    begin
      SetDefault;
      fVer := GetAttrValue(aAtts, sVer);
    end;

end;

procedure TBitBuilder.SetTagHandler(const aText: string);
var
  Bit: IBitDefine;
  Bits: IBits;
  Index: word;
begin
    case fSaxHandler.CurrentTagAttrType of
      tnName:
      begin
        fName := aText;
      end;

      tnIndex:
      begin
        Index := StrToIntDef(aText, 0);
        if Supports(fSaxHandler.IntfStack.Top, IBits, Bits) then
        begin
          Bits.Add(Index);
          if Bits.GetLastError = 0 then
          begin
            Bit := Bits[Index];
            fSaxHandler.IntfStack.Push(Bit);

            // Поместить в стек тегов новый тег
            fSaxHandler.TagStack.Push(TTypeTagAttrName.tnBitDefine);


          end;
        end;
      end;

      tnShortDescription:
      begin
        fShortDescription := aText;
      end;

      tnPara:
      begin
        if Supports(fSaxHandler.IntfStack.Top, IBitDefine, Bit) then
          Bit.Description.Add(aText);
      end;

    end;

end;

procedure TBitBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Bit: IBitDefine;
  Intf: IInterface;
begin
  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnBitDefine then
    // Получить интерфейс библиотеки на вершине стека интерфейсов
    if Supports(fSaxHandler.IntfStack.Top, IBitDefine, Bit) then
    begin
      Bit.Name := fName;
      Bit.ShortDescription := fShortDescription;
      Bit.Ver:= fVer;

      // Удалить из стека тегов
      fSaxHandler.TagStack.Pop(TypeTagAttrName);
      // Удалить из стека интерфейсов
      fSaxHandler.IntfStack.Pop(Intf);


    end;
end;

{ TGroupsBuilder }

procedure TGroupsBuilder.StartTagHandler(const aAtts: TSaxAttributes);
var
  Groups: IGroups;
  Index: integer;
begin

  if fSaxHandler.TagStack.Top = TTypeTagAttrName.tnEmpty then
  begin
    Groups := TSaxModuleDefineHandler(fSaxHandler).ModuleDefine.Configuration;

    Groups.ImageIndex := StrToIntDef(GetAttrValue(aAtts, sImageIndex), -1);

    fSaxHandler.IntfStack.Push(Groups);
    // Поместить в стек тегов новый тег
    fSaxHandler.TagStack.Push(TTypeTagAttrName.tnGncSet);

    Exit;
  end;

  if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnGncSet) then
  if Supports(fSaxHandler.IntfStack.Top, IGroups, Groups) then
  begin
    Index := Groups.GroupsList.AddGroups(GetAttrValue(aAtts, sName));

    Groups.GroupsList[Index].ImageIndex := StrToIntDef(GetAttrValue(aAtts, sImageIndex), -1);

    fSaxHandler.IntfStack.Push(Groups.GroupsList[Index]);

    // Поместить в стек тегов новый тег
    fSaxHandler.TagStack.Push(TTypeTagAttrName.tnGncSet);

  end;

end;

procedure TGroupsBuilder.SetTagHandler(const aText: string);
//var
//  Groups: IGroups;
begin
  //if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnImageIndex)
  //  and Supports(fSaxHandler.IntfStack.Top, IGroups, Groups) then
  //begin
  //  case fSaxHandler.CurrentTagAttrType of
  //    tnImageIndex:
  //    begin
  //      Groups.ImageIndex := StrToIntDef(aText, -1);
  //    end;
  //  end;
  //end;
end;

procedure TGroupsBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
  Intf:    Iinterface;
  Groups: IGroups;
begin

  // Удаляем объект из стека
  if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnGncSet) then
    if Supports(fSaxHandler.IntfStack.Top, IGroups, Groups) then
  begin
    fSaxHandler.IntfStack.Pop(Intf);
    fSaxHandler.TagStack.Pop(TypeTagAttrName);
  end;

end;

{ TGroupBuilder }

procedure TGroupBuilder.StartTagHandler(const aAtts: TSaxAttributes);
begin
  if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnGncSet) then
     fSaxHandler.TagStack.Push(TTypeTagAttrName.tnGncVar);

end;

procedure TGroupBuilder.SetTagHandler(const aText: string);
var
  VarDefine: IVarDefine;
  Groups: IGroups;

begin
  if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnGncVar) and
    Supports(fSaxHandler.IntfStack.Top, IGroups, Groups) then
  begin
    case fSaxHandler.CurrentTagAttrType of
      tnUID:
      begin
        VarDefine := TSaxModuleDefineHandler(fSaxHandler).ModuleDefine.
          Registers.FindByUid(aText);
        if VarDefine <> nil then
        begin
          Groups.Group.AddGroupItem(VarDefine);
        end;
      end;
    end;
  end;
end;

procedure TGroupBuilder.EndTagHandler;
var
  TypeTagAttrName: TTypeTagAttrName;
begin
  if (fSaxHandler.TagStack.Top = TTypeTagAttrName.tnGncVar) then
     fSaxHandler.TagStack.Pop(TypeTagAttrName);
end;

{$ENDREGION ModuleDefineBuilders}

end.

