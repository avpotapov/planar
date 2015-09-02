unit u2ColumnEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, VirtualTrees, Menus, lcltype, Dialogs,
  uVtBaseEditor;

type

  { T2ColumnData }

  T2ColumnData = class
  protected
    function GetShortDescription: string; virtual; abstract;
    function GetName: string; virtual; abstract;
    procedure SetName(const aName: string); virtual; abstract;
    procedure SetShortDescription(const aShortDescription: string); virtual; abstract;
  public
    property Name: string read GetName write SetName;
    property ShortDescription: string read GetShortDescription write SetShortDescription;
  end;

  { T2ColumnEditor }

  T2ColumnEditor = class(TVirtualStringTree)
  protected

    procedure SetOptions; virtual;
    procedure SetColumns; virtual;
    procedure ConstructPopupMenu; virtual;
    function GetData(const aNode: PVirtualNode): T2ColumnData; virtual;

    function DoKeyAction(var CharCode: word; var {%H-}Shift: TShiftState): boolean;
      override;
    procedure AddNewItem(Sender: TObject); virtual; abstract;
    procedure CloneItem(Sender: TObject); virtual; abstract;
    procedure DeleteItem(Sender: TObject); virtual; abstract;

    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): integer;
      override;
    procedure DoHeaderClick(Column: TColumnIndex; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    {%H-}TextType: TVSTTextType; var CellText: string); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string); override;
    function DoCreateEditor({%H-}Node: PVirtualNode;
    {%H-}Column: TColumnIndex): IVTEditLink; override;

    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoColumnDblClick(Column: TColumnIndex; Shift: TShiftState); override;

  public
    procedure AfterConstruction; override;


  end;

implementation

uses
  {%H-}uMenuHelper;

{ T2ColumnEditor }

procedure T2ColumnEditor.AfterConstruction;
begin
  inherited AfterConstruction;

  NodeDataSize := SizeOf(T2ColumnData);
  SetOptions;
  SetColumns;
  ConstructPopupMenu;
end;


procedure T2ColumnEditor.SetOptions;
begin
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus,
    {toFullRowSelect,} toRightClickSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toEditable];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines] +
    [toShowVertGridLines, toShowHorzGridLines];


  LineStyle := lsSolid;

  DefaultNodeHeight := 24;
end;

procedure T2ColumnEditor.SetColumns;
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
  Column.Text := 'Имя';
  Column.Width := 100;
  Column.Options := Column.Options + [coResizable];

  Column := Header.Columns.Add;
  Column.Text := 'Описание';
  Column.Width := 200;
  Column.Options := Column.Options + [coAutoSpring];
end;

procedure T2ColumnEditor.ConstructPopupMenu;
var
  Menu: TPopupMenu;
begin
  Menu := TPopupMenu.Create(Self);
  Menu.Parent := Self;
  PopupMenu := Menu;

  Menu.AddMenuItem('Добавить...', @AddNewItem);
  Menu.AddMenuItem('Клонировать...', @CloneItem);
  Menu.AddMenuItem('-');
  Menu.AddMenuItem('Удалить...', @DeleteItem);

end;

function T2ColumnEditor.GetData(const aNode: PVirtualNode): T2ColumnData;
var
  P: Pointer;
begin
  Result := nil;
  if aNode <> nil then
  begin
    P := GetNodeData(aNode);
    Result := T2ColumnData(P^);
  end;

end;

function T2ColumnEditor.DoKeyAction(var CharCode: word;
  var Shift: TShiftState): boolean;
var
  Column: TColumnIndex;
begin
  Result := True;
  case CharCode of
    VK_RETURN:
    	EditNode(FocusedNode, FocusedColumn);
    0:
    begin
      if FocusedColumn = Header.Columns.Count - 1 then
        Column := 0
      else
        Column := FocusedColumn + 1;
      FocusedColumn := Column;
      SetFocus;
    end;
  end;
end;

function T2ColumnEditor.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): integer;
begin
  Result := inherited DoCompare(Node1, Node2, Column);
  if Result = 0 then
    Result := CompareText(UpperCase(Text[Node1, Column]), UpperCase(Text[Node2, Column]));
end;

procedure T2ColumnEditor.DoHeaderClick(Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited DoHeaderClick(Column, Button, Shift, X, Y);
  with Header do
  begin
    SortColumn := Column;
    if SortDirection = sdAscending then
      SortDirection := sdDescending
    else
      SortDirection := sdAscending;
    Treeview.SortTree(SortColumn, SortDirection, False);
  end;

end;

procedure T2ColumnEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  case Column of
    0: CellText := GetData(Node).Name;
    1: CellText := GetData(Node).ShortDescription;
  end;
  inherited DoGetText(Node, Column, TextType, CellText);
end;

procedure T2ColumnEditor.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string);
begin
  case Column of
    0: GetData(Node).Name := CellText;
    1: GetData(Node).ShortDescription := CellText;
  end;
  inherited DoNewText(Node, Column, CellText);
end;

function T2ColumnEditor.DoCreateEditor(Node: PVirtualNode;
  Column: TColumnIndex): IVTEditLink;
begin
  Result := inherited DoCreateEditor(Node, Column);
end;

procedure T2ColumnEditor.DoFreeNode(Node: PVirtualNode);
begin
  GetData(Node).Free;
  inherited DoFreeNode(Node);
end;

procedure T2ColumnEditor.DoColumnDblClick(Column: TColumnIndex; Shift: TShiftState);
begin
  inherited DoColumnDblClick(Column, Shift);
  EditNode(FocusedNode, Column);
end;



end.
