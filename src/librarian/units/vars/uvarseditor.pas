unit uVarsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Dialogs, Windows, Forms,
  uLibrary,
  u2ColumnEditor,
  uVtBaseEditor;

type

  { TVarData }

  TVarData = class(T2ColumnData)
  private
    fVar: IVarDefine;
  protected
    function GetName: string; override;
    function GetShortDescription: string; override;
    function GetIndex: string;
    function GetMeasure: string;
    function GetMultipler: string;
    function GetVarType: string;

    procedure SetName(const aName: string); override;
    procedure SetShortDescription(const aShortDescription: string); override;
    procedure SetIndex(const aIndex: string);
    procedure SetMeasure(aMeasure: string);
    procedure SetMultipler(aMultipler: string);
    procedure SetVarType(aVarType: string);

  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  public

    property Index: string read GetIndex write SetIndex;
    property Measure: string read GetMeasure write SetMeasure;
    property VarType: string read GetVarType write SetVarType;
    property Multipler: string read GetMultipler write SetMultipler;

    property VarDefine: IVarDefine read fVar;
  end;

  TVarSelectEvent = procedure(Sender: TObject; const aVar: IVarDefine) of object;

  { TVarsEditor }

  TVarsEditor = class(T2ColumnEditor)
  private
    fVars: IVars;
    fOnVarSelect: TVarSelectEvent;
  private
    procedure SetVars(const aVars: IVars);
  protected
    procedure SetColumns; override;

    procedure ConstructNewVar(const aClonedVar: IVarDefine = nil);

    procedure AddNewItem(Sender: TObject); override;
    procedure CloneItem(Sender: TObject); override;
    procedure DeleteItem(Sender: TObject); override;

    procedure DoChange(Node: PVirtualNode); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): integer;
      override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    {%H-}TextType: TVSTTextType; var CellText: string); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string); override;
    function DoCreateEditor({%H-}Node: PVirtualNode;
    {%H-}Column: TColumnIndex): IVTEditLink; override;

  public
    property Vars: IVars read fVars write SetVars;
    property OnVarSelect: TVarSelectEvent read fOnVarSelect write fOnVarSelect;
  end;

implementation
{ TVarData }

constructor TVarData.Create(const aVar: IVarDefine);
begin
  inherited Create;
  fVar := aVar;
end;

function TVarData.GetName: string;
begin
  Result := fVar.Name;
end;

function TVarData.GetShortDescription: string;
begin
  Result := fVar.ShortDescription;
end;

function TVarData.GetIndex: string;
begin
  Result := IntToStr(fVar.Index);
end;

function TVarData.GetMeasure: string;
begin
  Result := fVar.Measure;
end;

function TVarData.GetMultipler: string;
begin
  Result := IntToStr(fVar.Multipler);
end;

function TVarData.GetVarType: string;
var
  FoundIndex: integer;
begin
  FoundIndex := VarTypes.IndexOfData(fVar.VarType);
  Result := VarTypes.Keys[FoundIndex];
end;

procedure TVarData.SetName(const aName: string);
begin
  fVar.Name := aName;
end;

procedure TVarData.SetShortDescription(const aShortDescription: string);
begin
  fVar.ShortDescription := aShortDescription;
end;

procedure TVarData.SetIndex(const aIndex: string);
begin
  fVar.Index := StrToIntDef(aIndex, 0);
end;

procedure TVarData.SetMeasure(aMeasure: string);
begin
  fVar.Measure := aMeasure;
end;

procedure TVarData.SetMultipler(aMultipler: string);
begin
  fVar.Multipler := StrToIntDef(aMultipler, 0);
end;

procedure TVarData.SetVarType(aVarType: string);
begin
  fVar.VarType := VarTypes[aVarType];
end;

{ TVarsEditor }

procedure TVarsEditor.SetVars(const aVars: IVars);
var
  P: Pointer;
  VarDefine: IVarDefine;
begin
  if fVars <> aVars then
    fVars := aVars;

  Clear;
  for P in fVars do
  begin
    VarDefine := fVars.ExtractData(P);
    AddChild(nil, TVarData.Create(VarDefine));
  end;

  Header.Treeview.SortTree(0, sdAscending, False);
  Header.SortColumn := 0;
  Header.SortDirection := sdAscending;
end;

procedure TVarsEditor.SetColumns;
var
  Column: TVirtualTreeColumn;
begin
  // Настройка заголовка компонента
  Header.Options := Header.Options + [hoVisible, hoAutoResize, hoAutoSpring];
  Header.MainColumn := 0;
  Header.AutoSizeIndex := 2;
  Header.Height := 20;

  // Колонки компонента
  Column := Header.Columns.Add;
  Column.Text := 'Индекс';
  Column.MinWidth := 75;
  Column.Options := Column.Options + [coResizable];

  Column := Header.Columns.Add;
  Column.Text := 'Имя';
  Column.MinWidth := 150;
  Column.Options := Column.Options + [coResizable];

  Column := Header.Columns.Add;
  Column.Text := 'Описание';
  Column.MinWidth := 150;
  Column.Options := Column.Options + [coResizable];

  Column := Header.Columns.Add;
  Column.Text := 'Измерение';
  Column.MinWidth := 100;
  Column.Options := Column.Options + [coResizable];

  Column := Header.Columns.Add;
  Column.Text := 'Тип';
  Column.MinWidth := 100;
  Column.Options := Column.Options + [coResizable];

  Column := Header.Columns.Add;
  Column.Text := 'Множитель';
  Column.MinWidth := 75;
  Column.Options := Column.Options + [coResizable];

end;

procedure TVarsEditor.DoChange(Node: PVirtualNode);
begin
  if Node <> nil then
  if Assigned(fOnVarSelect) then
    fOnVarSelect(Self, TVarData(GetData(Node)).VarDefine);
	inherited DoChange(Node);
end;

function TVarsEditor.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): integer;
begin
  case Column of
    0, 5: Result := StrToInt(Text[Node1, Column]) - StrToInt(Text[Node2, Column]);
    else
      Result := CompareText(UpperCase(Text[Node1, Column]),
        UpperCase(Text[Node2, Column]));
  end;
end;

procedure TVarsEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  case Column of
    0: CellText := TVarData(GetData(Node)).Index;
    1: CellText := TVarData(GetData(Node)).Name;
    2: CellText := TVarData(GetData(Node)).ShortDescription;
    3: CellText := TVarData(GetData(Node)).Measure;
    4: CellText := TVarData(GetData(Node)).VarType;
    5: CellText := TVarData(GetData(Node)).Multipler;
  end;
end;

procedure TVarsEditor.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string);
begin
  case Column of
    0: TVarData(GetData(Node)).Index := CellText;
    1: TVarData(GetData(Node)).Name := CellText;
    2: TVarData(GetData(Node)).ShortDescription := CellText;
    3: TVarData(GetData(Node)).Measure := CellText;
    4: TVarData(GetData(Node)).VarType := CellText;
    5: TVarData(GetData(Node)).Multipler := CellText;
  end;

end;

function TVarsEditor.DoCreateEditor(Node: PVirtualNode;
  Column: TColumnIndex): IVTEditLink;
var
  Strings: TStrings;
  I: Integer;
begin
  case Column of
    0: Result := TVtIntegerEditLink.Create(0, 65535, 1);

    4:
      begin
        Strings := TStringList.Create;
        try
          for I := 0 to VarTypes.Count - 1 do
          	Strings.Add(VarTypes.Keys[I]);
         	Result := TVtComboEditLink.Create(Strings);
        finally
          Strings.Free;
        end;
      end;

    5: Result := TVtIntegerEditLink.Create(1, 1000000, 1);
  	else
      Result := inherited DoCreateEditor(Node, Column);
  end;

end;

procedure TVarsEditor.ConstructNewVar(const aClonedVar: IVarDefine);
var
  IndexStr: string;
  Index: Integer;
  VarDefine: IVarDefine;

  Uid: string;

  Node: PVirtualNode;

begin
  IndexStr := '';
  if not InputQuery('Новый регистр', 'Введите индекс нового регистра', IndexStr) then
    Exit;

  try
    Uid := GetNewGuid;
    fVars.Add(Uid);

    if fVars.GetLastError <> 0 then
      raise Exception.Create('Регистр с данным UID уже существует');

    VarDefine := fVars[Uid];

    Index := StrToIntDef(IndexStr, 0);
    if aClonedVar <> nil then
    	VarDefine.Copy(aClonedVar);

    VarDefine.Index := Index;

    Node := AddChild(nil, TVarData.Create(VarDefine));
    Selected[Node] := True;
    ScrollIntoView(Node, True);

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания регистра')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure TVarsEditor.AddNewItem(Sender: TObject);
begin
  ConstructNewVar;
end;

procedure TVarsEditor.CloneItem(Sender: TObject);
begin
  ConstructNewVar(TVarData(GetData(FocusedNode)).VarDefine);
end;

procedure TVarsEditor.DeleteItem(Sender: TObject);
begin
  if MessageBox(Application.MainFormHandle,
      PChar(Utf8ToAnsi(Format('Удалить регистр ''%s''?',
      [TVarData(GetData(FocusedNode)).VarDefine.ShortDescription]))), PChar(Utf8ToAnsi('Удаление')),
      MB_ICONWARNING + MB_OKCANCEL) = idCancel then
      Exit;

  fVars.Remove(TVarData(GetData(FocusedNode)).VarDefine.Uid);
  DeleteNode(FocusedNode);
end;

end.



