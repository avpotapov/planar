unit uDetailedDeviceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  upropertyeditor, VirtualTrees, uConfiguratorData;

type

  { TDetailedDeviceForm }

  TDetailedDeviceForm = class(TDetailedForm)
    DeviceTree: TVirtualStringTree;

    procedure DeviceTreeColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; {%H-}Shift: TShiftState);
    procedure DeviceTreeCreateEditor(Sender: TBaseVirtualTree;
      {%H-}Node: PVirtualNode; {%H-}Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure DeviceTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DeviceTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DeviceTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure DeviceTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure DeviceTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; {%H-}Node: PVirtualNode; Column: TColumnIndex;
      {%H-}TextType: TVSTTextType);

  private
     fDeviceData: TDeviceData;

  private
      procedure PopulateDeviceTree;

  public
    procedure Load(const aContentData: array of TContentData); override;
    procedure Unload; override;
  end;



implementation

uses
  uDeviceFormData;

{$R *.lfm}

{ TDetailedDeviceForm }


procedure TDetailedDeviceForm.DeviceTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TDeviceBaseData);
end;

procedure TDetailedDeviceForm.DeviceTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  case Column of
    0: CellText := TDeviceBaseData(P^).Caption;
    1: CellText := TDeviceBaseData(P^).Value;
  end;
end;

procedure TDetailedDeviceForm.DeviceTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  case Column of
    0: TargetCanvas.Font.Style := [fsBold];
    1: TargetCanvas.Font.Style := [];
  end;
end;

procedure TDetailedDeviceForm.DeviceTreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditor.Create(Sender);
end;

procedure TDetailedDeviceForm.DeviceTreeColumnDblClick(
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
    if not TDeviceBaseData(P^).ReadOnly then
      Sender.EditNode(Node, Column);
  end;
end;

procedure TDetailedDeviceForm.DeviceTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  Allowed := (Column = 1) and not TDeviceBaseData(P^).ReadOnly;
end;

procedure TDetailedDeviceForm.DeviceTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  if TObject(P^) is TDeviceBaseData then
     TDeviceBaseData(P^).Free;
end;

procedure TDetailedDeviceForm.PopulateDeviceTree;
var
  S, N, N1: PVirtualNode;
begin
  DeviceTree.BeginUpdate;
  try
    S := DeviceTree.AddChild(nil, TModuleData.Create(fDeviceData));
    DeviceTree.AddChild(nil, TSlaveIdData.Create(fDeviceData));

    N := DeviceTree.AddChild(nil, TDeviceBaseData.Create('Аппаратная платформа', fDeviceData));
    N1 := DeviceTree.AddChild(N, TDeviceBaseData.Create('Тип', fDeviceData));
    DeviceTree.AddChild(N1, TTypeIdHardData.Create(fDeviceData));
    DeviceTree.AddChild(N1, TModificatorData.Create(fDeviceData));
    DeviceTree.Expanded[N1] := True;

    DeviceTree.AddChild(N, TSerialNumberData.Create(fDeviceData));
    DeviceTree.AddChild(N, TDateIdHardData.Create(fDeviceData));
    DeviceTree.Expanded[N] := True;

    N := DeviceTree.AddChild(nil, TDeviceBaseData.Create('Идентификатор прошивки', fDeviceData));
    DeviceTree.AddChild(N, TTypeFirmWareData.Create(fDeviceData));
    DeviceTree.AddChild(N, TVersionFirmWareData.Create(fDeviceData));
    DeviceTree.Expanded[N] := True;


    N := DeviceTree.AddChild(nil, TDeviceBaseData.Create('Проект', fDeviceData));
    DeviceTree.AddChild(N, TTypeProjectData.Create(fDeviceData));
    DeviceTree.AddChild(N, TVersionProjectData.Create(fDeviceData));
    DeviceTree.Expanded[N] := True;

    DeviceTree.Selected[S] := True;
  finally
    DeviceTree.EndUpdate;
  end;
end;

procedure TDetailedDeviceForm.Load(const aContentData: array of TContentData);
begin
  {$IFDEF DEBUG}
  Assert(Length(aContentData) = 1);
  {$ENDIF}

  if aContentData[0] is TDeviceData then
    fDeviceData := TDeviceData(aContentData[0]);

  {$IFDEF DEBUG}
  Assert(fDeviceData <> nil);
  {$ENDIF}

  PopulateDeviceTree;
end;

procedure TDetailedDeviceForm.Unload;
begin
  DeviceTree.Clear;
end;

end.

