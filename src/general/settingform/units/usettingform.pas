unit uSettingForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  upropertyeditor, VirtualTrees;

type



  {$REGION SettingForm}

  { TSettingForm }

  TSettingForm = class(TForm)
    SettingImageList: TImageList;
    SettingTree:      TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SettingTreeColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; {%H-}Shift: TShiftState);
    procedure SettingTreeCreateEditor(Sender: TBaseVirtualTree;
      {%H-}Node: PVirtualNode; {%H-}Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure SettingTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure SettingTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SettingTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; {%H-}Kind: TVTImageKind; Column: TColumnIndex;
      var {%H-}Ghosted: Boolean; var ImageIndex: Integer);
    procedure SettingTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure SettingTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure SettingTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex;
      {%H-}TextType: TVSTTextType);
  private
    procedure LoadSettings;
  public
    { public declarations }
  end;

 {$ENDREGION SettingForm}

implementation
uses
  ucustomeditor,
  uSettingFormData;


{$R *.lfm}


{$REGION SettingForm}

procedure TSettingForm.SettingTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TBaseData);
end;

procedure TSettingForm.SettingTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  case Column of
    0: CellText := TPropertyData(P^).Caption;
    1: CellText := TPropertyData(P^).Value;
  end;
end;

procedure TSettingForm.SettingTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  if TBaseData(P^).TypeEditor = TBaseData.TTypeEditor.teNone then
    TargetCanvas.Font.Style := [fsBold]
  else
    TargetCanvas.Font.Style := [];
end;

procedure TSettingForm.LoadSettings;
var
  N1, N2: PVirtualNode;
begin

  //////////////////////////////////////////////////////////////////////////////
  // MODBUS
  // Загрузить данные в SettingTree
  N1 := SettingTree.AddChild(nil, TTreeData.Create('Связь', TTreeData.TTypeImage.tiModbus));
  N2 := SettingTree.AddChild(N1, TTreeData.Create('Последовательный порт', TTreeData.TTypeImage.tiRtu));

  SettingTree.AddChild(N2, TBaudRate.Create);
  SettingTree.AddChild(N2, TParity.Create);
  SettingTree.AddChild(N2, TByteSize.Create);
  SettingTree.AddChild(N2, TStopBits.Create);
  SettingTree.AddChild(N2, TThreeAndHalf.Create);

  N2 := SettingTree.AddChild(N1, TTreeData.Create('TCP - клиент', TTreeData.TTypeImage.tiClient));

  SettingTree.AddChild(N2, TIpAddress.Create);
  SettingTree.AddChild(N2, TPort.Create);

  N2 := SettingTree.AddChild(N1, TTreeData.Create('TCP - сервер', TTreeData.TTypeImage.tiServer));

  SettingTree.AddChild(N2, TServerPort.Create);
  SettingTree.AddChild(N2, TAddress.Create);
  SettingTree.AddChild(N2, TKeepAlive.Create);

  //////////////////////////////////////////////////////////////////////////////
  // Конфигуратор
  N1 := SettingTree.AddChild(nil, TTreeData.Create('Конфигуратор', TTreeData.TTypeImage.tiConfig));

  N2 := SettingTree.AddChild(N1, TTreeData.Create('Таймауты', TTreeData.TTypeImage.tiTimeout));

  SettingTree.AddChild(N2, TTimeOut.Create);
  SettingTree.AddChild(N2, TResponseTimeout.Create);
  SettingTree.AddChild(N2, TBackupTimeout.Create);

  SettingTree.AddChild(N1, TRepeats.Create);

  //////////////////////////////////////////////////////////////////////////////
  // БИБЛИОТЕКА
  N1 := SettingTree.AddChild(nil, TTreeData.Create('Библиотека', TTreeData.TTypeImage.tiXmlLibrary));

  N2 := SettingTree.AddChild(N1, TTreeData.Create('Разработчика', TTreeData.TTypeImage.tiDeveloper));

  SettingTree.AddChild(N2, TDeveloperLibrary.Create);
  SettingTree.AddChild(N2, TDeveloperImage.Create);

  N2 := SettingTree.AddChild(N1, TTreeData.Create('Пользователя', TTreeData.TTypeImage.tiUser));

  SettingTree.AddChild(N2, TUserLibrary.Create);
  SettingTree.AddChild(N2, TUserImage.Create);

end;

procedure TSettingForm.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TSettingForm.SettingTreeColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var
  Node: PVirtualNode;
  P: Pointer;
begin
  if Column = 1 then
  begin
    Node := Sender.FocusedNode;
    if Node = nil then
      Exit;
    P := Sender.GetNodeData(Node);
    if TBaseData(P^).TypeEditor <> TBaseData.TTypeEditor.teNone then
      Sender.EditNode(Node, Column);
  end;
end;

procedure TSettingForm.SettingTreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditor.Create(Sender);
end;

procedure TSettingForm.SettingTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  Allowed := (Column = 1) and (TBaseData(P^).TypeEditor <> TBaseData.TTypeEditor.teNone);
end;

procedure TSettingForm.SettingTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  TBaseData(P^).Free;
end;

procedure TSettingForm.SettingTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  P: Pointer;
begin
  if Column = 0 then
  begin
    P := Sender.GetNodeData(Node);
    if TBaseData(P^) is TTreeData then
      ImageIndex := Ord(TTreeData(P^).Image);
  end;
end;

procedure TSettingForm.FormHide(Sender: TObject);
begin
  SettingTree.Clear;
end;

procedure TSettingForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;
end;

{$ENDREGION SettingForm}


end.
