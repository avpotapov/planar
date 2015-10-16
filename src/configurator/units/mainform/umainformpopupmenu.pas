unit uMainFormPopupMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Registry, Forms, Controls, Dialogs,
  uConfiguratorData;

type
{$REGION ModbusPopupMenuFactory}

  TModbusPopupMenuFactory = class
  private
    fSerialList: TStrings;
    fOnClick: TNotifyEvent;
    fMenu: TPopupMenu;
  private
    {%H-}constructor Create(const aMenu: TPopupMenu; const aOnClick: TNotifyEvent);
    procedure CreateSerialList;
    procedure CreateMenuItems;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class procedure Setup(const aMenu: TPopupMenu; const aOnClick: TNotifyEvent);
  end;

{$ENDREGION ModbusPopupMenuFactory}


{$REGION TContentTreeProxyPopupMenu}

  { TContentTreeProxyPopupMenu }

  TContentTreeProxyPopupMenu = class
  private
    fMenu: TPopupMenu;
    fDeviceData: TDeviceData;
    fOnChangeLinkStatus: TNotifyEvent;
    fOnAddDevice: TNotifyEvent;
    fOnRemoveNode: TNotifyEvent;

  private
    procedure ResetDevice(Sender: TObject);
    procedure ShowSerialForm(Sender: TObject);
    procedure ViewModbusMenu(const aModbusData: TModbusData);
    procedure AddMenuItem(const aCaption: string; const aOnClick: TNotifyEvent = nil);
  public
    constructor Create(const aMenu: TPopupMenu);

  public
    procedure Setup(aOnChangeLinkStatus, aOnAddDevice, aOnRemoveNode: TNotifyEvent);

    procedure View(const aContentData: TContentData);
  end;

{$ENDREGION TContentTreeProxyPopupMenu}


implementation

uses
  uModbus, uSerialForm;

{$REGION ModbusPopupMenuFactory}

constructor TModbusPopupMenuFactory.Create(const aMenu: TPopupMenu;
  const aOnClick: TNotifyEvent);
begin
  inherited Create;
  fMenu := aMenu;
  fOnClick := aOnClick;
  fSerialList := TStringList.Create;
end;

procedure TModbusPopupMenuFactory.CreateSerialList;
const
  KEY = 'Hardware\DeviceMap\SerialComm';
var
  I, ListCount: integer;
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY, False);
    Reg.GetValueNames(fSerialList);
    ListCount := fSerialList.Count - 1;
    for I := 0 to ListCount do
      fSerialList[I] := Reg.ReadString(fSerialList[I]);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

procedure TModbusPopupMenuFactory.CreateMenuItems;
const
  SERIAL_IMAGEINDEX = 0;
  TCP_IMAGEINDEX = 1;
  TCP = 'TCP';
var
  S: string;
  MenuItem: TMenuItem;
begin
  fMenu.Items.Clear;

  // RTU
  for S in fSerialList do
  begin
    MenuItem := TMenuItem.Create(Application);
    MenuItem.Caption := S;
    MenuItem.ImageIndex := SERIAL_IMAGEINDEX;
    MenuItem.OnClick := fOnClick;
    fMenu.Items.Add(MenuItem);
  end;

  // TCP
  MenuItem := TMenuItem.Create(Application);
  MenuItem.Caption := TCP;
  MenuItem.ImageIndex := TCP_IMAGEINDEX;
  MenuItem.OnClick := fOnClick;
  fMenu.Items.Add(MenuItem);
end;

procedure TModbusPopupMenuFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateSerialList;
end;

destructor TModbusPopupMenuFactory.Destroy;
begin
  FreeAndNil(fSerialList);
  inherited Destroy;
end;

class procedure TModbusPopupMenuFactory.Setup(const aMenu: TPopupMenu;
  const aOnClick: TNotifyEvent);
begin
  with TModbusPopupMenuFactory.Create(aMenu, aOnClick) do
    try
      CreateMenuItems;
    finally
      Free;
    end;
end;

{$ENDREGION ModbusPopupMenuFactory}


{$REGION ContentTreeProxyPopupMenu}

procedure TContentTreeProxyPopupMenu.ViewModbusMenu(const aModbusData: TModbusData);
begin
  if aModbusData.Controller.TypeController <> TTypeController.mbAcTcp then
  begin
    case aModbusData.Controller.IsOpen of
      True: AddMenuItem('Выключить связь', fOnChangeLinkStatus);
      False: AddMenuItem('Включить связь', fOnChangeLinkStatus);
    end;
    AddMenuItem('-');
  end;

  AddMenuItem('Добавить устройство', fOnAddDevice);
  AddMenuItem('-');

  case aModbusData.AutoScan.IsScanning of
    True:
      AddMenuItem('Прекратить автопоиск устройств', @aModbusData.AutoScan.StopScan);
    False:
      AddMenuItem('Автопоиск устройств', @aModbusData.AutoScan.StartScan);
  end;
  AddMenuItem('-');

  AddMenuItem('Удалить канал связи', fOnRemoveNode);

end;

procedure TContentTreeProxyPopupMenu.AddMenuItem(const aCaption: string;
  const aOnClick: TNotifyEvent);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Application);
  MenuItem.Caption := aCaption;
  MenuItem.OnClick := aOnClick;
  fMenu.Items.Add(MenuItem);
end;

constructor TContentTreeProxyPopupMenu.Create(const aMenu: TPopupMenu);
begin
  inherited Create;
  fMenu := aMenu;
end;

procedure TContentTreeProxyPopupMenu.Setup(aOnChangeLinkStatus,
  aOnAddDevice, aOnRemoveNode: TNotifyEvent);
begin
  fOnChangeLinkStatus := aOnChangeLinkStatus;
  fOnAddDevice := aOnAddDevice;
  fOnRemoveNode := aOnRemoveNode;
end;

procedure TContentTreeProxyPopupMenu.View(const aContentData: TContentData);
begin
  fMenu.Items.Clear;
  if aContentData is TModbusData then
    ViewModbusMenu(TModbusData(aContentData));
  if aContentData is TDeviceData then
  begin

    fDeviceData := aContentData as TDeviceData;
    AddMenuItem('Серийный номер...', @ShowSerialForm);
    AddMenuItem('Перезагрузить устройство', @ResetDevice);
    AddMenuItem('Удалить устройство', fOnRemoveNode);
  end;
end;

procedure TContentTreeProxyPopupMenu.ResetDevice(Sender: TObject);
var
  Frame: IFrame;
begin
  Screen.Cursor := crHourGlass;
  try
    FRame := uModbus.ResetApplication(fDeviceData.SlaveId, 1000);
    FRame.Priority := TPriority.prHigh;

    if (fDeviceData.ContentSet[1] is TModbusData) then
      (fDeviceData.ContentSet[1] as TModbusData).Controller.InQueue(FRame);
    Frame.Responded;

    sleep(1000);

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TContentTreeProxyPopupMenu.ShowSerialForm(Sender: TObject);
begin
  with TSerialForm.Create(nil) do
    try
      Load([fDeviceData.ContentSet[0], fDeviceData.ContentSet[1]]);
      ShowModal;
    finally
      Free;
    end;
end;


{$ENDREGION ContentTreeProxyPopupMenu}
end.




