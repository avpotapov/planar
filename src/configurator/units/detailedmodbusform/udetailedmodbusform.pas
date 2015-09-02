unit uDetailedModbusForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  upropertyeditor, VirtualTrees, uConfiguratorData;

type

  { TDetailedModbusForm }

  TDetailedModbusForm = class(TDetailedForm)
    SettingTree: TVirtualStringTree;
    procedure SettingTreeColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; {%H-}Shift: TShiftState);
    procedure SettingTreeCreateEditor(Sender: TBaseVirtualTree;
      {%H-}Node: PVirtualNode; {%H-}Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure SettingTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure SettingTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SettingTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure SettingTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure SettingTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; {%H-}Node: PVirtualNode; Column: TColumnIndex;
      {%H-}TextType: TVSTTextType);

  private
    fModbusData: TModbusData;

  private
    procedure PopulateSettingTree;
  public
    procedure Load(const aContentData: array of TContentData); override;
    procedure Unload; override;
  end;


implementation
uses
  uModbus,
  uModbusFormData;


{$R *.lfm}

{ TDetailedModbusForm }

procedure TDetailedModbusForm.SettingTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TModbusBaseData);
end;

procedure TDetailedModbusForm.SettingTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  case Column of
    0: CellText := TModbusBaseData(P^).Caption;
    1: CellText := TModbusBaseData(P^).Value;
  end;
end;

procedure TDetailedModbusForm.SettingTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
    case Column of
    0: TargetCanvas.Font.Style := [fsBold];
    1: TargetCanvas.Font.Style := [];
  end;
end;

procedure TDetailedModbusForm.PopulateSettingTree;
var
  ModbusBaseData: TModbusBaseData;
begin
  SettingTree.BeginUpdate;
  try
    case fModbusData.Controller.TypeController of
    mbRtu:
      begin
        SettingTree.AddChild(nil, TBaudRate.Create(fModbusData));
        SettingTree.AddChild(nil, TParity.Create(fModbusData));
        SettingTree.AddChild(nil, TByteSize.Create(fModbusData));
        SettingTree.AddChild(nil, TStopBits.Create(fModbusData));
        SettingTree.AddChild(nil, TThreeAndHalf.Create(fModbusData));
      end;
    mbAcTcp:
      begin
        ModbusBaseData := TIpAddress.Create(fModbusData);
        ModbusBaseData.ReadOnly := True;
        SettingTree.AddChild(nil, ModbusBaseData);
        ModbusBaseData := TPort.Create(fModbusData);
        ModbusBaseData.ReadOnly := True;
        SettingTree.AddChild(nil, ModbusBaseData);
      end;
    mbCnTcp:
      begin
        SettingTree.AddChild(nil, TIpAddress.Create(fModbusData));
        SettingTree.AddChild(nil, TPort.Create(fModbusData));
      end;
    end;

  finally
    SettingTree.EndUpdate;
  end;
end;

procedure TDetailedModbusForm.SettingTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  if TObject(P^) is TModbusBaseData then
     TModbusBaseData(P^).Free;
end;

procedure TDetailedModbusForm.SettingTreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditor.Create(Sender);
end;

procedure TDetailedModbusForm.SettingTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  Allowed := (Column = 1) and not TModbusBaseData(P^).ReadOnly;
end;

procedure TDetailedModbusForm.SettingTreeColumnDblClick(
  Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
var
  Node: PVirtualNode;
  P:    Pointer;
begin
  if Column = 1 then
  begin
    Node := Sender.FocusedNode;
    if Node = nil then
      Exit;
    P := Sender.GetNodeData(Node);
    if not TModbusBaseData(P^).ReadOnly then
      Sender.EditNode(Node, Column);
  end;

end;

procedure TDetailedModbusForm.Load(const aContentData: array of TContentData);
begin
  {$IFDEF DEBUG}
  Assert(Length(aContentData) = 1);
  {$ENDIF}

  if aContentData[0] is TModbusData then
    fModbusData := TModbusData(aContentData[0]);

  {$IFDEF DEBUG}
  Assert(fModbusData <> nil);
  {$ENDIF}
  PopulateSettingTree;
end;

procedure TDetailedModbusForm.Unload;
begin
  SettingTree.Clear;
end;

end.

