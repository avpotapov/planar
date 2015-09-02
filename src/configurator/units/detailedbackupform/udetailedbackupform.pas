unit uDetailedBackupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, fgl,
  Menus, VirtualTrees, uConfiguratorData, uBackupFormData, uLibrary;

type

  { TDetailedBackupForm }

  TGroupItemList = specialize TFpgList<TGroupItemBackupData>;

  TDetailedBackupForm = class(TDetailedForm)
    ResetMenuItem: TMenuItem;
    SaveBackupVarsToolButton: TToolButton;
    VarPopupMenu: TPopupMenu;
    VarTree:      TVirtualStringTree;
    VarImageList: TImageList;
    ToolButtonImageList: TImageList;
    ContentToolBar: TToolBar;
    DelimiterToolButton: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResetMenuItemClick(Sender: TObject);
    procedure VarTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VarTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VarTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; {%H-}Column: TColumnIndex;
      var {%H-}Ghosted: Boolean; var ImageIndex: Integer);
    procedure VarTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VarTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
    {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
  private
    fModbusData:    TModbusData;
    fDeviceData:    TDeviceData;
    fBackupData:    TBackupData;
    fGroupItemList: TGroupItemList;
  private
    procedure PopulateVarTree;
  public
    procedure Load(const aContentData: array of TContentData); override;
    procedure Unload; override;
  end;



implementation

{$R *.lfm}

//type
  //TTGroupItemListHelper = class helper for  TGroupItemList
  //  public
  //    function BinarySearch()
  //end;


{ TDetailedBackupForm }

procedure TDetailedBackupForm.VarTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TBaseBackupData);
end;

procedure TDetailedBackupForm.VarTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  TBaseBackupData(P^).Free;
end;

procedure TDetailedBackupForm.ResetMenuItemClick(Sender: TObject);
var
  GroupItemBackupData: TGroupItemBackupData;
begin
  for GroupItemBackupData in fGroupItemList do
    GroupItemBackupData.Checked := False;
end;

procedure TDetailedBackupForm.FormCreate(Sender: TObject);
begin
  fGroupItemList := TGroupItemList.Create;
end;

procedure TDetailedBackupForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fGroupItemList);
end;

procedure TDetailedBackupForm.VarTreeChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  if Node^.CheckType = TCheckType.ctCheckBox then
  begin
    P := Sender.GetNodeData(Node);
    if TBaseBackupData(P^) is TGroupItemBackupData then
      TGroupItemBackupData(P^).Checked := not TGroupItemBackupData(P^).Checked;
  end;
end;

procedure TDetailedBackupForm.VarTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  case Kind of
    ikNormal: ImageIndex   := Ord(TBaseBackupData(P^).GetImage);
    ikSelected: ImageIndex := Ord(TBaseBackupData(P^).GetSelected);
  end;
end;

procedure TDetailedBackupForm.VarTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  CellText := TBaseBackupData(P^).Caption;
end;

procedure TDetailedBackupForm.PopulateVarTree;

  procedure AddConfigs(const aNode: PVirtualNode; const aGroups: IGroups);
  var
    Groups:    IGroups;
    Node:      PVirtualNode;
    GroupItem: IGroupItem;
    GroupItemBackupData: TGroupItemBackupData;
  begin
    for Groups in aGroups.GroupsList do
    begin
      Node := VarTree.AddChild(aNode, TGroupsBackupData.Create(Groups));
      AddConfigs(Node, Groups);
    end;

    for GroupItem in aGroups.GetGroup do
    begin
      GroupItemBackupData := TGroupItemBackupData.Create(GroupItem);
      Node := VarTree.AddChild(aNode, GroupItemBackupData);
      Node^.CheckType := TCheckType.ctCheckBox;
      GroupItemBackupData.OwnerNode := Node;
      GroupItemBackupData.Tree := VarTree;
      // Список элементов группы
      fGroupItemList.Add(GroupItemBackupData);
    end;
  end;

begin
  {$IFDEF DEBUG}
  Assert(fDeviceData <> nil);
  {$ENDIF}
  VarTree.BeginUpdate;
  try
    AddConfigs(nil, fDeviceData.Module.ModuleDefine.Configuration);
    fGroupItemList.Sort(@GroupItemCompare);
  finally
    VarTree.EndUpdate;
  end;
end;


procedure TDetailedBackupForm.Load(const aContentData: array of TContentData);
var
  I: Integer;
begin
  {$IFDEF DEBUG}
  Assert(Length(aContentData) = 3);
  {$ENDIF}

  // Инициализация
  for I := Low(aContentData) to High(aContentData) do
  begin
    if aContentData[I] is TModbusData then
      fModbusData := aContentData[I] as TModbusData;
    if aContentData[I] is TDeviceData then
      fDeviceData := aContentData[I] as TDeviceData;
    if aContentData[I] is TBackupData then
      fBackupData := aContentData[I] as TBackupData;
  end;

  PopulateVarTree;
end;

procedure TDetailedBackupForm.Unload;
begin
  fGroupItemList.Clear;
end;

end.
