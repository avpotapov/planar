unit udetailedgroupform;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}// Альтернатива -dDEBUG
{$C+}// Альтернатива Включить Assert

interface


uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, upropertyeditor, Windows,
  uConfiguratorData,
  VirtualTrees,
  uLibrary,
  uFrameListFactory,
  uGroupItemTreeNodeFactory,
  uModbus;

const
  WM_COUNT  = WM_USER + 1;
  WM_UPDATE = WM_USER + 2;


type


  {$REGION DetailedConfigsForm }

  { TDetailedGroupForm }

  TDetailedGroupForm = class(TDetailedForm)
    GroupItemTree: TVirtualStringTree;
    DescMemo:    TMemo;
    HSplitter:   TSplitter;
    procedure GroupItemTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GroupItemTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GroupItemTreeChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var {%H-}NewState: TCheckState; var Allowed: Boolean);
    procedure GroupItemTreeColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; {%H-}Shift: TShiftState);
    procedure GroupItemTreeCreateEditor(Sender: TBaseVirtualTree;
    {%H-}Node: PVirtualNode; {%H-}Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure GroupItemTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure GroupItemTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GroupItemTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure GroupItemTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure WmUpdate(var {%H-}Message: TMessage); message WM_Update;

  private
    fConfigsTreeNodeFactory: TGroupItemTreeNodeFactory;
    fFrameListFactory: TFrameListFactory;
    fFrameList: TFrameList;

    fModbusData:  TModbusData;
    fDeviceData:  TDeviceData;
    fConfigsData: TGroupsData;

    fClosed:   Integer;
    fThreadId: TThreadId;

  private
    procedure PopulateConfigsTree;
    procedure CreateFrameList;
    procedure StartPoll;
    procedure ClosePoll;
  public
    function IsClosed: Boolean;
    procedure Load(const aContentData: array of TContentData); override;
    procedure Unload; override;


  end;

  {$ENDREGION DetailedConfigsForm }



implementation


{$R *.lfm}

{$REGION Цикл опроса регистров}


function ThreadFnc(aParameter: Pointer): Integer;
var
  Form:  TDetailedGroupForm;
  Frame: IFrame;
begin
  Result := 0;
  {$IFDEF DEBUG}
  Assert(aParameter <> nil);
  Assert(TObject(aParameter) is TDetailedGroupForm);
  {$ENDIF}
  Form := TDetailedGroupForm(aParameter);

  while not Form.IsClosed do
  begin
    for Frame in Form.fFrameList do
    begin

      // Пока не отправлен запрос
      while not Form.fModbusData.Controller.InQueue(Frame) do
      begin
        Sleep(20);

        if Form.IsClosed then
          Exit;

      end;

      // Обработка ответа
      if Frame.Responded then
      begin
        Form.fDeviceData.Map.WriteData(Swap(PWord(@Frame.RequestPdu^[1])^),
          Frame.ResponsePdu);



        PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
      end
      else
        PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);


      if Form.IsClosed then
        Exit;

      SendMessage(Form.Handle, WM_UPDATE, 0, 0);

    end;
  end;
end;


{$ENDREGION Цикл опроса регистров}


{$REGION DetailedConfigsForm}

procedure TDetailedGroupForm.WmUpdate(var Message: TMessage);
begin
  GroupItemTree.Invalidate;
end;

function TDetailedGroupForm.IsClosed: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fClosed, 0, 0) = 1;
end;


procedure TDetailedGroupForm.GroupItemTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TGroupItemData);
end;

procedure TDetailedGroupForm.GroupItemTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  case Column of
    0: CellText := TVarData(P^).Caption;
    1: CellText := TVarData(P^).Value;
  end;
end;

procedure TDetailedGroupForm.FormCreate(Sender: TObject);
begin
  fConfigsTreeNodeFactory := TGroupItemTreeNodeFactory.Create(GroupItemTree);
  fFrameListFactory := TFrameListFactory.Create;
  fFrameList := TFrameList.Create;
  fThreadId  := 0;
  fClosed    := 1;
end;

procedure TDetailedGroupForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fConfigsTreeNodeFactory);
  FreeAndNil(fFrameListFactory);
  FreeAndNil(fFrameList);
end;

procedure TDetailedGroupForm.GroupItemTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  TGroupItemData(P^).Free;
end;

procedure TDetailedGroupForm.GroupItemTreeChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  if TGroupItemData(P^) is TBitData then
    TBitData(P^).Value:= 'Inverted';
end;

procedure TDetailedGroupForm.GroupItemTreeChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  if (P <> nil) and (TGroupItemData(P^) is TBitData) then
    Allowed := not TBitData(P^).ReadOnly;
end;

procedure TDetailedGroupForm.GroupItemTreeColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
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
    if not TGroupItemData(P^).ReadOnly then
      Sender.EditNode(Node, Column);
  end;

end;

procedure TDetailedGroupForm.GroupItemTreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditor.Create(Sender);
end;

procedure TDetailedGroupForm.GroupItemTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  Allowed := (Column = 1) and not TGroupItemData(P^).ReadOnly;
end;

procedure TDetailedGroupForm.GroupItemTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: Pointer;
begin
  if Node <> nil then
  begin
    P := Sender.GetNodeData(Node);
    TGroupItemData(P^).SetDescription(DescMemo.Lines);
 end;
end;

procedure TDetailedGroupForm.PopulateConfigsTree;
var
  GroupItem: IGroupItem;
begin
  {$IFDEF DEBUG}
  Assert(fConfigsData <> nil);
  Assert(fDeviceData <> nil);
  {$ENDIF}

  GroupItemTree.BeginUpdate;
  try

    for GroupItem in fConfigsData.Groups.Group do
      fConfigsTreeNodeFactory.GetNode(GroupItem.VarDefine, fDeviceData.Map, fModbusData.Controller, fDeviceData.SlaveId);

    // Выделить первый узел
    if GroupItemTree.RootNode^.ChildCount > 0 then
      GroupItemTree.Selected[GroupItemTree.RootNode^.FirstChild] := True;

  finally
    GroupItemTree.EndUpdate;
  end;

end;

procedure TDetailedGroupForm.CreateFrameList;
var
  GroupItem: IGroupItem;
begin
  for GroupItem in fConfigsData.Groups.Group do
    fFrameListFactory.VarList.Add(GroupItem.VarDefine);
  fFrameListFactory.CreateFrameList(fDeviceData.SlaveId, fFrameList);
end;

procedure TDetailedGroupForm.StartPoll;
begin
  Windows.InterLockedExchange(fClosed, 0);
  fThreadId := BeginThread(@ThreadFnc, Self);
end;

procedure TDetailedGroupForm.ClosePoll;
begin
  if fThreadId <> 0 then
  begin
    Windows.InterLockedExchange(fClosed, 1);
    WaitForThreadTerminate(fThreadId, 1000);
    CloseThread(fThreadId);
    fThreadId := 0;
  end;
end;

procedure TDetailedGroupForm.Load(const aContentData: array of TContentData);
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
    if aContentData[I] is TGroupsData then
      fConfigsData := aContentData[I] as TGroupsData;
  end;

  // Заполнить таблицу данными
  PopulateConfigsTree;
  // Создать список запросов Modbus
  CreateFrameList;
  // Запустить опрос регистров
  StartPoll;
end;

procedure TDetailedGroupForm.Unload;
begin
  // Остановить опрос
  ClosePoll;
  // Очистить
  GroupItemTree.Clear;
  DescMemo.Lines.Clear;
  fFrameListFactory.VarList.Clear;
  fFrameList.Clear;
end;

{$ENDREGION DetailedConfigsForm}

end.
