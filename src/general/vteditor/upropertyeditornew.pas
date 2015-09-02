unit uPropertyEditorNew;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lcltype, VirtualTrees, uVtBaseEditor;

type

  (*
		Шаблонный метод

		Наследники должны переопределить некоторые шаги алгоритма
		(GetValue, SetValue), не изменяя его структуру в целом.

    Участники:

    1. Abstract class (абстрактный класс) - определяет абстрактные операции,
    замещаемые в наследниках для реализации шагов алгоритма; реализует шаблонный
    метод, определяющий скелет алгоритма.
    Шаблонный метод вызывает замещаемые и другие, определенные в Abstract class,
    операции.

    2. Concrete class (конкретный класс) - реализует замещаемые операции
    необходимым для данной реализации способом.
    Concrete class предполагает, что инвариантные шаги алгоритма будут выполнены
    в AbstractClass.

  *)

  { TPropertyData }

  TPropertyData = class(TVtBaseData)
  private
    fKey: string;
  protected
    function GetValue: string; virtual; abstract;
    procedure SetValue(const aValue: string); virtual; abstract;
  public
    property Key: string read fKey write fKey;
    property Value: string read GetValue write SetValue;
  end;


  { TPropertyEditor }

  TPropertyEditor = class(TVirtualStringTree)
  private
    procedure SetOptions;
    procedure SetColumns;

  protected
    function DoKeyAction(var CharCode: word; var {%H-}Shift: TShiftState): boolean; override;
    function GetData(const aNode: PVirtualNode): TPropertyData;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    {%H-}TextType: TVSTTextType; var CellText: string); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String); override;
    function DoCreateEditor({%H-}Node: PVirtualNode;
    {%H-}Column: TColumnIndex): IVTEditLink; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: boolean); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoColumnDblClick(Column: TColumnIndex;
      Shift: TShiftState); override;
  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation


{ TPropertyEditor }

constructor TPropertyEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  NodeDataSize := SizeOf(TPropertyData);
  SetColumns;
  SetOptions;
end;

procedure TPropertyEditor.DoFreeNode(Node: PVirtualNode);
begin
  GetData(Node).Free;
  inherited DoFreeNode(Node);
end;

procedure TPropertyEditor.DoColumnDblClick(Column: TColumnIndex;
  Shift: TShiftState);
begin
  inherited DoColumnDblClick(Column, Shift);
  EditNode(FocusedNode, Column);
end;

procedure TPropertyEditor.SetOptions;
begin
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus, toRightClickSelect];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines] +
    [toShowVertGridLines, toShowHorzGridLines];
  LineStyle := lsSolid;
  DefaultNodeHeight := 24;
end;

procedure TPropertyEditor.SetColumns;
var
  Column: TVirtualTreeColumn;
begin
  // Настройка заголовка компонента
  Header.Options := Header.Options + [hoVisible, hoAutoResize, hoAutoSpring];
  Header.MainColumn := 0;
  Header.AutoSizeIndex := 1;
  Header.Height := 20;

  // Колонки компонента
  Column := Header.Columns.Add;
  Column.Text := 'Параметр';
  Column.Width := 175;
  Column.Options := Column.Options + [coResizable];

  Column := Header.Columns.Add;
  Column.Text := 'Значение';
  Column.Width := 100;
  Column.Options := Column.Options + [coAutoSpring];

end;

function TPropertyEditor.DoKeyAction(var CharCode: word; var Shift: TShiftState
  ): boolean;
var
  Column: TColumnIndex;
  Node: PVirtualNode;
begin
  Result := True;
  case CharCode of
    VK_RETURN:  EditNode(FocusedNode, FocusedColumn);
    0:
      begin
        if FocusedNode^.NextSibling = nil then
        	FocusedNode := FocusedNode^.Parent^.FirstChild
        else
          FocusedNode := FocusedNode^.NextSibling;
        Selected[FocusedNode] := True;
        //FocusedColumn := 1;
        SetFocus;
      end;
  end;
end;

function TPropertyEditor.GetData(const aNode: PVirtualNode): TPropertyData;
var
  P: Pointer;
begin
  Result := nil;
  if aNode <> nil then
  begin
    P := GetNodeData(aNode);
    Result := TPropertyData(P^);
  end;
end;

procedure TPropertyEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  case Column of
    0: CellText := GetData(Node).Key;
    1: CellText := GetData(Node).Value;
  end;
  inherited DoGetText(Node, Column, TextType, CellText);
end;

procedure TPropertyEditor.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String);
begin
  case Column of
    0: GetData(Node).Key := CellText;
    1: GetData(Node).Value := CellText;
  end;
  inherited DoNewText(Node, Column, CellText);
end;

function TPropertyEditor.DoCreateEditor(Node: PVirtualNode;
  Column: TColumnIndex): IVTEditLink;
begin
  Result := GetData(Node).EditLink;
  if Result = nil then
  	 Result := inherited DoCreateEditor(Node, Column);
end;

procedure TPropertyEditor.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: boolean);
begin
  inherited DoCanEdit(Node, Column, Allowed);
  Allowed := (Column = 1) and not GetData(Node).ReadOnly;
end;

end.

