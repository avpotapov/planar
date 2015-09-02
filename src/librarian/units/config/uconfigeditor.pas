unit uConfigEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ImgList, ActiveX, Controls, Dialogs, lcltype,
  Menus,
  uLibrary;

type

  { TConfigData }

  TConfigData = class
  protected
    function GetCaption: string; virtual; abstract;
    function GetImageIndex: integer; virtual;
  public
    property Caption: string read GetCaption;
    property ImageIndex: integer read GetImageIndex;
  end;

  { TGroupsData }

  TGroupsData = class(TConfigData)
  private
    fGroups: IGroups;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;
  public
    constructor Create(const aGroups: IGroups); reintroduce;
  end;

  { TGroupItemData }

  TGroupItemData = class(TConfigData)
  private
    fGroupItem: IGroupItem;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;
  public
    constructor Create(const aGroupItem: IGroupItem); reintroduce;
  end;

  { IDragDropHandler }

  IDragDropHandler = interface
    ['{7F9AB1D0-D914-48CD-86D0-59A27EC39917}']
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree);
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree);
    procedure SetNextHandler(const aDragDropHandler: IDragDropHandler);
  end;

  { TDragDropHandler }

  TDragDropHandler = class(TInterfacedObject, IDragDropHandler)
  protected
    fNextHandler: IDragDropHandler;
    fCondition: boolean;
    fAttMode: TVTNodeAttachMode;
  public
    constructor Create;
  protected
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree); virtual;
  public
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree); virtual;
    procedure SetNextHandler(const aDragDropHandler: IDragDropHandler);
  end;

  { TItem2GroupHandler }

  TItem2GroupHandler = class(TDragDropHandler)
  protected
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree); override;
  public
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree); override;
  end;

  { TItem2ItemHandler }

  TItem2ItemHandler = class(TDragDropHandler)
  protected
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree); override;
  public
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree); override;
  end;


  { TGroups2GroupsHandler }

  TGroups2GroupsHandler = class(TDragDropHandler)
  protected
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree); override;
  public
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree); override;
  end;

  { TGroups2ItemHandler }

  TGroups2ItemHandler = class(TDragDropHandler)
  protected
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree); override;
  public
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree); override;
  end;

  { TVars2ItemHandler }

  TVars2ItemHandler = class(TDragDropHandler)
  protected
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree); override;
  public
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree); override;
  end;

  { TVars2GroupsHandler }

  TVars2GroupsHandler = class(TDragDropHandler)
  protected
    procedure DoDragDrop(const aSource, aTarget: TVirtualStringTree); override;
  public
    procedure DragDrop(const aSource, aTarget: TVirtualStringTree); override;
  end;


  TNodeSelectEvent = procedure(Sender: TObject; const aConfig: TConfigData) of object;

  { TConfigEditor }
  TConfigEditor = class(TVirtualStringTree)
  private
    fConfig: IGroups;
    fDragDropHandler: IDragDropHandler;
    fOnNodeSelect: TNodeSelectEvent;
    function GetData(const aNode: PVirtualNode): TConfigData; virtual;
    procedure SetDragDropHandlers;
    procedure DeleteItems(Sender: TObject);
    procedure AddGroups(Sender: TObject);
    procedure AddRootGroups(Sender: TObject);
    procedure EditGroups(Sender: TObject);
    procedure ConstructPopupMenu;
  protected
    function DoKeyAction(var CharCode: word; var {%H-}Shift: TShiftState): boolean; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoChange(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    {%H-}TextType: TVSTTextType; var CellText: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: boolean; var Index: integer): TCustomImageList;
      override;
    function DoBeforeDrag({%H-}Node: PVirtualNode; {%H-}Column: TColumnIndex): boolean;
      override;
    function DoDragOver(Source: TObject; {%H-}Shift: TShiftState;
    {%H-}State: TDragState; const {%H-}Pt: TPoint; {%H-}Mode: TDropMode;
      var {%H-}Effect: longword): boolean;
      override;
    procedure DoColumnDblClick(Column: TColumnIndex; Shift: TShiftState); override;

    procedure DoDragDrop(Source: TObject; {%H-}DataObject: IDataObject;
    {%H-}Formats: TFormatArray; {%H-}Shift: TShiftState; const {%H-}Pt: TPoint;
      var {%H-}Effect: longword; {%H-}Mode: TDropMode); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
     const CellText: string); override;
  public
    procedure LoadConfig(const aConfig: IGroups);
    procedure AfterConstruction; override;
    property OnNodeSelect: TNodeSelectEvent read fOnNodeSelect write fOnNodeSelect;
  end;




implementation

uses
  uRegistersEditor,
	{%H-}uMenuHelper;

{ TVars2ItemHandler }

function FindDuplicate(const aGroup: IGroup; const aVar: IVarDefine): boolean;
var
  GroupItem: IGroupItem;
begin
  Result := False;
  for GroupItem in aGroup do
    if GroupItem.VarDefine.Uid = aVar.Uid then
    begin
      Result := True;
      Break;
    end;
end;

{ TVars2GroupsHandler }

procedure TVars2GroupsHandler.DoDragDrop(const aSource, aTarget: TVirtualStringTree);

var
  PSource, PTarget: PVirtualNode;
  TargetGroup: IGroup;
  Index: integer;
  VarDefine: IVarDefine;
begin
  PSource := TRegistersEditor(aSource).GetFirstSelected();
  PTarget := aTarget.DropTargetNode;
  TargetGroup := TGroupsData(TConfigEditor(aTarget).GetData(PTarget)).fGroups.Group;

  while Assigned(PSource) do
  begin
    VarDefine := (TRegistersEditor(aSource).GetData(PSource) as TVarData).VarDefine;
    if not FindDuplicate(TargetGroup, VarDefine) then
    begin
      Index := TargetGroup.AddGroupItem(VarDefine);
      aTarget.AddChild(PTarget, TGroupItemData.Create(TargetGroup[Index]));
    end;
    PSource := TRegistersEditor(aSource).GetNextSelected(PSource);
  end;
end;

procedure TVars2GroupsHandler.DragDrop(const aSource, aTarget: TVirtualStringTree);
begin
  fCondition := (aSource is TRegistersEditor) and
    (TRegistersEditor(aSource).GetData(aSource.FocusedNode) is TVarData) and
    (aTarget is TConfigEditor) and
    (TConfigEditor(aTarget).GetData(aTarget.DropTargetNode) is TGroupsData);

  inherited DragDrop(aSource, aTarget);
end;

procedure TVars2ItemHandler.DoDragDrop(const aSource, aTarget: TVirtualStringTree);
var
  PSource, PTarget: PVirtualNode;
  TargetGroup: IGroup;
  Index: integer;
  VarDefine: IVarDefine;
begin
  PSource := TRegistersEditor(aSource).GetFirstSelected();
  PTarget := aTarget.DropTargetNode^.Parent;
  TargetGroup := TGroupsData(TConfigEditor(aTarget).GetData(PTarget)).fGroups.Group;

  while Assigned(PSource) do
  begin
    VarDefine := (TRegistersEditor(aSource).GetData(PSource) as TVarData).VarDefine;
    if not FindDuplicate(TargetGroup, VarDefine) then
    begin
      Index := TargetGroup.AddGroupItem(VarDefine);
      aTarget.AddChild(PTarget, TGroupItemData.Create(TargetGroup[Index]));
    end;
    PSource := TRegistersEditor(aSource).GetNextSelected(PSource);
  end;
end;

procedure TVars2ItemHandler.DragDrop(const aSource, aTarget: TVirtualStringTree);
begin
  fCondition := (aSource is TRegistersEditor) and
    (TRegistersEditor(aSource).GetData(aSource.FocusedNode) is TVarData) and
    (aTarget is TConfigEditor) and
    (TConfigEditor(aTarget).GetData(aTarget.DropTargetNode) is TGroupItemData);

  inherited DragDrop(aSource, aTarget);
end;

{ TGroups2ItemHandler }

procedure TGroups2ItemHandler.DoDragDrop(const aSource, aTarget: TVirtualStringTree);
var
  SourceGroupsList, TargetGroupsList: IGroupsList;
  Groups: IGroups;
begin

  Groups := TGroupsData(TConfigEditor(aSource).GetData(aSource.FocusedNode)).fGroups;

  if aSource.FocusedNode^.Parent = aSource.RootNode then
    SourceGroupsList := TConfigEditor(aSource).fConfig.GroupsList
  else
    SourceGroupsList := TGroupsData(TConfigEditor(aSource).GetData(
      aSource.FocusedNode^.Parent)).fGroups.GroupsList;

  TargetGroupsList := TGroupsData(TConfigEditor(aTarget).GetData(
    aTarget.DropTargetNode^.Parent)).fGroups.GroupsList;


  SourceGroupsList.Remove(Groups);
  TargetGroupsList.Add(Groups);


  inherited DoDragDrop(aSource, aTarget);
end;

procedure TGroups2ItemHandler.DragDrop(const aSource, aTarget: TVirtualStringTree);
begin
  fCondition := (aSource is TConfigEditor) and
    (TConfigEditor(aSource).GetData(aSource.FocusedNode) is TGroupsData) and
    (aTarget is TConfigEditor) and
    (TConfigEditor(aTarget).GetData(aTarget.DropTargetNode) is TGroupItemData);

  inherited DragDrop(aSource, aTarget);
end;

{ TGroups2GroupsHandler }

procedure TGroups2GroupsHandler.DoDragDrop(const aSource, aTarget: TVirtualStringTree);
var
  SourceGroupsList, TargetGroupsList: IGroupsList;
  Groups: IGroups;
  Index: integer;
begin

  Groups := TGroupsData(TConfigEditor(aSource).GetData(aSource.FocusedNode)).fGroups;

  if aSource.FocusedNode^.Parent = aSource.RootNode then
    SourceGroupsList := TConfigEditor(aSource).fConfig.GroupsList
  else
    SourceGroupsList := TGroupsData(TConfigEditor(aSource).GetData(
      aSource.FocusedNode^.Parent)).fGroups.GroupsList;

  if aTarget.DropTargetNode^.Parent = aTarget.RootNode then
    TargetGroupsList := TConfigEditor(aTarget).fConfig.GroupsList
  else
    TargetGroupsList := TGroupsData(TConfigEditor(aTarget).GetData(
      aTarget.DropTargetNode^.Parent)).fGroups.GroupsList;

  Index := TargetGroupsList.IndexOf(
    TGroupsData(TConfigEditor(aTarget).GetData(aTarget.DropTargetNode)).fGroups);

  SourceGroupsList.Remove(Groups);
  TargetGroupsList.Insert(Index, Groups);

  inherited DoDragDrop(aSource, aTarget);
end;

procedure TGroups2GroupsHandler.DragDrop(const aSource, aTarget: TVirtualStringTree);
begin
  fCondition := (aSource is TConfigEditor) and
    (TConfigEditor(aSource).GetData(aSource.FocusedNode) is TGroupsData) and
    (aTarget is TConfigEditor) and
    (TConfigEditor(aTarget).GetData(aTarget.DropTargetNode) is TGroupsData);

  inherited DragDrop(aSource, aTarget);
end;

{ TItem2ItemHandler }

procedure TItem2ItemHandler.DoDragDrop(const aSource, aTarget: TVirtualStringTree);
var
  PSource: PVirtualNode;
  SourceGroup, TargetGroup: IGroup;
  GroupItem: IGroupItem;
  Index: integer;
begin
  PSource := TConfigEditor(aSource).GetFirstSelected();
  SourceGroup := TGroupsData(TConfigEditor(aSource).GetData(aSource.FocusedNode^.Parent)).fGroups.Group;

  TargetGroup := TGroupsData(TConfigEditor(aTarget).GetData(
    aTarget.DropTargetNode^.Parent)).fGroups.Group;

  while Assigned(PSource) do
  begin
    GroupItem := TGroupItemData(TConfigEditor(aSource).GetData(PSource)).fGroupItem;

    Index := TargetGroup.IndexOf(
      TGroupItemData(TConfigEditor(aTarget).GetData(aTarget.DropTargetNode)).fGroupItem);

    SourceGroup.Remove(GroupItem);
    TargetGroup.Insert(Index, GroupItem);
    aTarget.MoveTo(PSource, aTarget.DropTargetNode, fAttMode, False);

    PSource := TConfigEditor(aSource).GetNextSelected(PSource);
  end;

end;

procedure TItem2ItemHandler.DragDrop(const aSource, aTarget: TVirtualStringTree);
begin
  fCondition := (aSource is TConfigEditor) and
    (TConfigEditor(aSource).GetData(aSource.FocusedNode) is TGroupItemData) and
    (aTarget is TConfigEditor) and
    (TConfigEditor(aTarget).GetData(aTarget.DropTargetNode) is TGroupItemData);

  inherited DragDrop(aSource, aTarget);
end;

{ TItem2GroupHandler }

procedure TItem2GroupHandler.DoDragDrop(const aSource, aTarget: TVirtualStringTree);
var
  PSource: PVirtualNode;
  SourceGroup, TargetGroup: IGroup;
  GroupItem: IGroupItem;

begin

  PSource := aSource.GetFirstSelected();
  SourceGroup := TGroupsData(TConfigEditor(aSource).GetData(aSource.FocusedNode^.Parent)).fGroups.Group;

  TargetGroup := TGroupsData(TConfigEditor(aTarget).GetData(
    aTarget.DropTargetNode)).fGroups.Group;

  while Assigned(PSource) do
  begin
    GroupItem := TGroupItemData(TConfigEditor(aSource).GetData(PSource)).fGroupItem;

    SourceGroup.Remove(GroupItem);
    TargetGroup.Add(GroupItem);

    aTarget.MoveTo(PSource, aTarget.DropTargetNode, amAddChildLast, False);

    PSource := aSource.GetNextSelected(PSource);
  end;

end;

procedure TItem2GroupHandler.DragDrop(const aSource, aTarget: TVirtualStringTree);
begin

  fCondition := (aSource is TConfigEditor) and
    (TConfigEditor(aSource).GetData(aSource.FocusedNode) is TGroupItemData) and
    (aTarget is TConfigEditor) and
    (TConfigEditor(aTarget).GetData(aTarget.DropTargetNode) is TGroupsData);

  inherited DragDrop(aSource, aTarget);

end;


{ TDragDropHandler }

constructor TDragDropHandler.Create;
begin
  inherited Create;
  fCondition := False;
  fNextHandler := nil;
  fAttMode := amInsertBefore;
end;

procedure TDragDropHandler.SetNextHandler(const aDragDropHandler: IDragDropHandler);
begin
  fNextHandler := aDragDropHandler;
end;

procedure TDragDropHandler.DragDrop(const aSource, aTarget: TVirtualStringTree);
begin
  case fCondition of
    True: DoDragDrop(aSource, aTarget);
    False:
      if fNextHandler <> nil then
        fNextHandler.DragDrop(aSource, aTarget);
  end;
end;

procedure TDragDropHandler.DoDragDrop(const aSource, aTarget: TVirtualStringTree);
begin
  aTarget.MoveTo(aSource.FocusedNode, aTarget.DropTargetNode, fAttMode, False);
end;


{ TGroupItemData }

function TGroupItemData.GetCaption: string;
begin
  Result := fGroupItem.VarDefine.ShortDescription;
end;

function TGroupItemData.GetImageIndex: integer;
begin
  Result := 1;
end;

constructor TGroupItemData.Create(const aGroupItem: IGroupItem);
begin
  inherited Create;
  fGroupItem := aGroupItem;
end;

{ TGroupsData }

function TGroupsData.GetCaption: string;
begin
  Result := fGroups.ShortDescription;
end;

function TGroupsData.GetImageIndex: integer;
begin
  Result := 0;
end;

constructor TGroupsData.Create(const aGroups: IGroups);
begin
  inherited Create;
  fGroups := aGroups;
end;

{ TConfigEditor }

procedure TConfigEditor.LoadConfig(const aConfig: IGroups);

  procedure AddGroupsNode(const aNode: PVirtualNode; const aGroups: IGroups);
  var
    Groups: IGroups;
    GroupItem: IGroupItem;
    ConfigData: TConfigData;
    GroupsNode: PVirtualNode;
  begin
    for Groups in aGroups.GroupsList do
    begin
      ConfigData := TGroupsData.Create(Groups);
      GroupsNode := AddChild(aNode, ConfigData);

      AddGroupsNode(GroupsNode, Groups);
    end;

    for GroupItem in aGroups.Group do
    begin
      ConfigData := TGroupItemData.Create(GroupItem);
      AddChild(aNode, TGroupItemData.Create(GroupItem));
    end;
  end;

var
  Groups: IGroups;
  ConfigData: TConfigData;
  GroupsNode: PVirtualNode;
begin
  fConfig := aConfig;

  for Groups in aConfig.GroupsList do
  begin
    ConfigData := TGroupsData.Create(Groups);
    GroupsNode := AddChild(nil, ConfigData);
    AddGroupsNode(GroupsNode, Groups);
  end;

end;

procedure TConfigEditor.AfterConstruction;

begin
  inherited AfterConstruction;
  TreeOptions.SelectionOptions :=
    TreeOptions.SelectionOptions + [toExtendedFocus, toFullRowSelect,
    toRightClickSelect, toMultiSelect];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines] +
    [toShowVertGridLines, toShowHorzGridLines];
  LineStyle := lsSolid;

  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toCheckSupport];

  DefaultNodeHeight := 24;

  NodeDataSize := SizeOf(TConfigData);

  SetDragDropHandlers;
  ConstructPopupMenu;
end;

function TConfigEditor.GetData(const aNode: PVirtualNode): TConfigData;
var
  P: Pointer;
begin
  Result := nil;
  if (aNode = nil) or (aNode = RootNode) then
    Exit;
  P := GetNodeData(aNode);
  Result := TConfigData(P^);
end;

procedure TConfigEditor.SetDragDropHandlers;
var
  Item2ItemHandler, Groups2GroupsHandler, Groups2ItemHandler,
  Vars2ItemHandler, Vars2GroupsHandler: IDragDropHandler;
begin
  fDragDropHandler := TItem2GroupHandler.Create as IDragDropHandler;
  Item2ItemHandler := TItem2ItemHandler.Create as IDragDropHandler;
  Groups2GroupsHandler := TGroups2GroupsHandler.Create as IDragDropHandler;
  Groups2ItemHandler := TGroups2ItemHandler.Create as IDragDropHandler;
  Vars2ItemHandler := TVars2ItemHandler.Create as IDragDropHandler;
  Vars2GroupsHandler := TVars2GroupsHandler.Create as IDragDropHandler;

  fDragDropHandler.SetNextHandler(Item2ItemHandler);
  Item2ItemHandler.SetNextHandler(Groups2GroupsHandler);
  Groups2GroupsHandler.SetNextHandler(Groups2ItemHandler);
  Groups2ItemHandler.SetNextHandler(Vars2ItemHandler);
  Vars2ItemHandler.SetNextHandler(Vars2GroupsHandler);
end;

procedure TConfigEditor.DeleteItems(Sender: TObject);
var
  PSource: PVirtualNode;
  ParentGroups: IGroups;
begin

  PSource := GetFirstSelected;
  while PSource <> nil do
  begin

    if GetData(PSource) is TGroupsData then
    begin
      if PSource^.Parent = RootNode then
        ParentGroups := fConfig
      else
        ParentGroups := TGroupsData(GetData(PSource^.Parent)).fGroups;
      ParentGroups.GetGroupsList.Remove(TGroupsData(GetData(PSource)).fGroups);
    end;

    if GetData(PSource) is TGroupItemData then
    begin
      ParentGroups := TGroupsData(GetData(PSource^.Parent)).fGroups;
      ParentGroups.Group.Remove(TGroupItemData(GetData(PSource)).fGroupItem);
    end;

    PSource := GetNextSelected(PSource);
  end;

  DeleteSelectedNodes;
end;

procedure TConfigEditor.AddGroups(Sender: TObject);
var
  NameStr: string;
  Index: integer;
  Groups: IGroups;
  Node: PVirtualNode;
begin
  Node := FocusedNode;
  NameStr := '';
  if not InputQuery('Новая группа переменных', 'Введите название группы переменных',
    NameStr) then
    Exit;
  if Node = nil then
  begin
    Index := fConfig.GroupsList.AddGroups(NameStr);
    Groups := fConfig.GroupsList[Index];
  end;

  if GetData(Node) is TGroupsData then
  begin
    Index := TGroupsData(GetData(Node)).fGroups.GroupsList.AddGroups(NameStr);
    Groups := TGroupsData(GetData(Node)).fGroups.GroupsList[Index];
  end;

  if GetData(Node) is TGroupItemData then
  begin
    Node := Node^.Parent;
    Index := TGroupsData(GetData(Node)).fGroups.GroupsList.AddGroups(NameStr);
    Groups := TGroupsData(GetData(Node)).fGroups.GroupsList[Index];
  end;

  Node := AddChild(Node, TGroupsData.Create(Groups));
 // Selected[Node] := True;

end;

procedure TConfigEditor.AddRootGroups(Sender: TObject);
begin
  FocusedNode := nil;
  AddGroups(nil);
end;

procedure TConfigEditor.EditGroups(Sender: TObject);
begin
  EditNode(FocusedNode, -1);
end;

procedure TConfigEditor.ConstructPopupMenu;
var
  Menu: TPopupMenu;
begin
  Menu := TPopupMenu.Create(Self);
  Menu.Parent := Self;
  PopupMenu := Menu;

  Menu.AddMenuItem('Новая группа...', @AddGroups);
  Menu.AddMenuItem('Новая корневая группа...', @AddRootGroups);
  Menu.AddMenuItem('Редактировать', @EditGroups);
  Menu.AddMenuItem('-');
  Menu.AddMenuItem('Удалить...', @DeleteItems);

end;

function TConfigEditor.DoKeyAction(var CharCode: word;
  var Shift: TShiftState): boolean;
begin
  Result := True;
  case CharCode of
    VK_DELETE: DeleteItems(nil);
    VK_INSERT: AddGroups(nil);
    VK_RETURN:  EditNode(FocusedNode, -1);
  end;
end;

procedure TConfigEditor.DoFreeNode(Node: PVirtualNode);
begin
  GetData(Node).Free;
  inherited DoFreeNode(Node);
end;

procedure TConfigEditor.DoChange(Node: PVirtualNode);
begin
  inherited DoChange(Node);
  if Node <> nil then
    if Assigned(fOnNodeSelect) then
      fOnNodeSelect(Self, GetData(Node));
end;

procedure TConfigEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  CellText := GetData(Node).Caption;
  inherited DoGetText(Node, Column, TextType, CellText);
end;

function TConfigEditor.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var Index: integer): TCustomImageList;
begin
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
  Index := GetData(Node).ImageIndex;
end;

function TConfigEditor.DoBeforeDrag(Node: PVirtualNode; Column: TColumnIndex): boolean;
begin
  Result := True;
end;

function TConfigEditor.DoDragOver(Source: TObject; Shift: TShiftState;
  State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: longword): boolean;
begin
  Result := (Source = Self) or (Source is TRegistersEditor);
end;

procedure TConfigEditor.DoColumnDblClick(Column: TColumnIndex;
  Shift: TShiftState);
begin
  inherited DoColumnDblClick(Column, Shift);
  EditNode(FocusedNode, Column);
end;

procedure TConfigEditor.DoDragDrop(Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint;
  var Effect: longword; Mode: TDropMode);
begin
  fDragDropHandler.DragDrop(TVirtualStringTree(Source), Self);
end;

procedure TConfigEditor.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  inherited DoCanEdit(Node, Column, Allowed);
  Allowed := GetData(Node) is TGroupsData;
end;

procedure TConfigEditor.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string);
begin
  if GetData(Node) is TGroupsData then
  	TGroupsData(GetData(Node)).fGroups.ShortDescription := CellText;
  inherited DoNewText(Node, Column, CellText);
end;

{ TConfigData }

function TConfigData.GetImageIndex: integer;
begin
  Result := -1;
end;

end.













