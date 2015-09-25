unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, VirtualTrees, Windows,
  uLibrary,
  uModbus,
  uDetailedProxyForm,
  uMainFormPopupMenu,
  uHoldingsForm;

const
  WM_COUNT = WM_USER + 1;

type

  {$REGION MainFrom}

  { TMainForm }

  TMainForm = class(TForm)
    ContentImageList: TImageList;
    ContentTreeImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    SeparatorMenuItem: TMenuItem;
    ReloadMenuItem: TMenuItem;
    ModbusImageList: TImageList;
    MainMenu: TMainMenu;
    ApplicationMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    ContentPanel: TPanel;
    DetailedPanel: TPanel;
    ModbusPopupMenu: TPopupMenu;
    ContentTreePopupMenu: TPopupMenu;
    SettingsMenuItem: TMenuItem;
    MainSplitter: TSplitter;
    StatusBar: TStatusBar;
    ContentTree: TVirtualStringTree;
    ContentToolBar: TToolBar;
    ModbusToolButton: TToolButton;
    Splitter1ToolButton: TToolButton;
    ChangeModbusStatusToolButton: TToolButton;
    Splitter2ToolButton: TToolButton;
    StartStopServerToolButton: TToolButton;
    Splitter3ToolButton: TToolButton;
    DeviceToolButton: TToolButton;
    SignatureToolButton: TToolButton;
    ToolButton1: TToolButton;
    SaveHoldingsToolButton: TToolButton;
    RestoreHoldingsToolButton: TToolButton;

    procedure ContentTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ContentTreeContextPopup(Sender: TObject; {%H-}MousePos: TPoint;
      var {%H-}Handled: boolean);
    procedure ContentTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ContentTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; {%H-}Column: TColumnIndex;
      var {%H-}Ghosted: boolean; var ImageIndex: integer);
    procedure ContentTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: integer);

    procedure ContentTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
    {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: string);

    procedure DeviceToolButtonClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ModbusToolButtonClick(Sender: TObject);
    procedure ChangeModbusStatusToolButtonClick(Sender: TObject);
    procedure ReloadMenuItemClick(Sender: TObject);
    procedure RestoreHoldingsToolButtonClick(Sender: TObject);
    procedure SaveHoldingsToolButtonClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);
    procedure SignatureToolButtonClick(Sender: TObject);
    procedure StartStopServerToolButtonClick(Sender: TObject);

  private
    fServer: IServer;
    fLibrary: ILibrary;
    // Счетчик
    fSuccess, fBad: longint;

    fDetailedProxyForm: TDetailedProxyForm;
    fContentTreeProxyPopupMenu: TContentTreeProxyPopupMenu;

  private
    procedure AddModbus(Sender: TObject);
    procedure RemoveNode(Sender: TObject);
    procedure ChangeModbusStatusStyle(const aController: IController);
    procedure ChangeServerStatusStyle;
    procedure SaveHoldings(const aTypeOperation: TTypeOperation);


    procedure WmCount(var Message: TMessage); message WM_COUNT;
    // Принимает новый контроллер, созданный сервером
    procedure WmNewClient(var Message: TMessage); message WM_NEW_CLIENT; // uModbus

  end;

  {$ENDREGION MainFrom}



var
  MainForm: TMainForm;

implementation

uses
  uSettingForm,
  uDeviceForm,
  uConfiguratorData,
  uContentBuilder,
  uSetting,
  uSplashForm;

{$R *.lfm}


{$REGION VirtualTreeViewHelper}
type

  TTypeNode = (tnModbus, tnDevice);

  { TVirtualTreeViewHelper }
  TVirtualTreeViewHelper = class Helper for TBaseVirtualTree
  public
    function GetParentNode(aTypeNode: TTypeNode;
      aCurrentNode: PVirtualNode): PVirtualNode;
    function GetData(const aNode: PVirtualNode): TContentData;
  end;


function TVirtualTreeViewHelper.GetParentNode(aTypeNode: TTypeNode;
  aCurrentNode: PVirtualNode): PVirtualNode;
var
  Level: cardinal;
begin
  Level := Ord(aTypeNode);
  Result := nil;
  if GetNodeLevel(aCurrentNode) < Level then
    Exit;
  Result := aCurrentNode;
  while GetNodeLevel(Result) <> Level do
    Result := Result^.Parent;
end;

function TVirtualTreeViewHelper.GetData(const aNode: PVirtualNode
  ): TContentData;
var
  P: Pointer;
begin
  P := GetNodeData(aNode);
  if TObject(P^) is TContentData then
    Result := TContentData(P^);
end;


{$ENDREGION VirtualTreeViewHelper}

{$REGION StatusBarHelper}
type

  { TStatusBarHelper }
  TStatusBarHelper = class Helper for TStatusBar
  public
    procedure Info(const aMsg: string);
    procedure Count(const aMsg: string);
  end;


procedure TStatusBarHelper.Info(const aMsg: string);
begin
  Panels[0].Text := aMsg;
end;

procedure TStatusBarHelper.Count(const aMsg: string);
begin
  Panels[1].Text := aMsg;
end;

{$ENDREGION StatusBarHelper}

{$REGION MainFrom}
procedure TMainForm.FormCreate(Sender: TObject);
var
  SplashForm: TSplashForm;
begin

  fSuccess := 0;
  fBad := 0;

  SplashForm := TSplashForm.Create('configurator.png');
  try

    SplashForm.Show;
    SplashForm.Repaint;

    // Список доступных каналов связи
    TModbusPopupMenuFactory.Setup(ModbusPopupMenu, @AddModbus);

    // Загрузить библиотеку
    try
      fLibrary := GetLibrary([GetSetting.DeveloperLibrary + '\module.jlf',
        GetSetting.UserLibrary + '\module.jlf']);

    except
      MessageBox(Handle, PChar(Utf8ToAnsi('Ошибка загрузки файла библиотеки')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    end;

    // Форма детализации
    fDetailedProxyForm := TDetailedProxyForm.Create(DetailedPanel);

    // Контексное меню для ContentTree
    fContentTreeProxyPopupMenu :=
      TContentTreeProxyPopupMenu.Create(ContentTreePopupMenu);
    fContentTreeProxyPopupMenu.Setup(@ChangeModbusStatusToolButtonClick,
      @DeviceToolButtonClick, @RemoveNode);

  finally
    SplashForm.Hide;
    SplashForm.Free;
  end;
end;



procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.ContentTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: integer);
begin
  NodeDataSize := SizeOf(TContentData);
end;

procedure TMainForm.ContentTreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
var
  P: Pointer;
  Node: PVirtualNode;
begin
  Node := ContentTree.FocusedNode;

  if Node <> nil then
  begin
    P := ContentTree.GetNodeData(Node);
    fContentTreeProxyPopupMenu.View(TContentData(P^));
  end;
end;

procedure TMainForm.ContentTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  TContentData(P^).Free;
end;

procedure TMainForm.ContentTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ModbusNode: PVirtualNode;
  DeviceNode: PVirtualNode;
  P: Pointer;
begin
  ModbusNode := nil;
  DeviceNode := nil;

  // Управление связи
  ModbusNode := Sender.GetParentNode(TTypeNode.tnModbus, Node);
  if ModbusNode <> nil then
  begin
    P := Sender.GetNodeData(ModbusNode);
    if TContentData(P^) is TModbusData then
    begin
      ChangeModbusStatusToolButton.Enabled :=
        TModbusData(P^).Controller.TypeController <> TTypeController.mbAcTcp;
      ChangeModbusStatusStyle(TModbusData(P^).Controller);
    end;
  end;

  DeviceToolButton.Enabled := ModbusNode <> nil;
  DeviceNode := Sender.GetParentNode(TTypeNode.tnDevice, Node);
  SignatureToolButton.Enabled := DeviceNode <> nil;
  SaveHoldingsToolButton.Enabled := DeviceNode <> nil;
  RestoreHoldingsToolButton.Enabled := DeviceNode <> nil;

  if Node <> nil then
  begin
    P := ContentTree.GetNodeData(Node);
    fDetailedProxyForm.View(TContentData(P^).ContentSet);
  end;

end;

procedure TMainForm.ContentTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: integer);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  case Kind of
    ikNormal:
    begin
      if TContentData(P^) is TGroupsData then
        ImageIndex := TGroupsData(P^).GroupsImageIndex
      else
        ImageIndex := Ord(TContentData(P^).Image);
    end;
    ikSelected:
    begin
      if TContentData(P^) is TGroupsData then
        ImageIndex := TGroupsData(P^).GroupsImageIndex
      else
        ImageIndex := Ord(TContentData(P^).Selected);
    end;
  end;
end;

procedure TMainForm.ContentTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  P: Pointer;
begin
  P := Sender.GetNodeData(Node);
  CellText := TContentData(P^).Caption;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(fDetailedProxyForm);
  FreeAndNil(fContentTreeProxyPopupMenu);
end;

procedure TMainForm.ChangeModbusStatusStyle(const aController: IController);
const
  CONTROLLER_CLOSE = 1;
  CONTROLLER_OPEN = 2;
begin
  case aController.IsOpen of
    True:
    begin
      ChangeModbusStatusToolButton.Down := True;
      ChangeModbusStatusToolButton.ImageIndex := CONTROLLER_OPEN;
      StatusBar.Info('Контроллер связи включен');
      ChangeModbusStatusToolButton.Hint := 'Выключить канал связи';
    end;
    False:
    begin
      ChangeModbusStatusToolButton.Down := False;
      ChangeModbusStatusToolButton.ImageIndex := CONTROLLER_CLOSE;
      StatusBar.Info('Контроллер связи выключен');
      ChangeModbusStatusToolButton.Hint := 'Включить контроллер связи';
    end;
  end;

end;

procedure TMainForm.ChangeServerStatusStyle;
const
  SERVER_OFF = 3;
  SERVER_ON = 4;
var
  IsActive: boolean;
begin
  IsActive := (fServer <> nil) and fServer.IsActive;
  case IsActive of
    True:
    begin
      StartStopServerToolButton.Down := True;
      StartStopServerToolButton.ImageIndex := SERVER_ON;
      StatusBar.Info('Сервер запущен');
      StartStopServerToolButton.Hint := 'Остановить сервер';
    end;
    False:
    begin
      StartStopServerToolButton.Down := False;
      StartStopServerToolButton.ImageIndex := SERVER_OFF;
      StatusBar.Info('Сервер остановлен');
      StartStopServerToolButton.Hint := 'Запустить сервер';
    end;
  end;

end;

procedure TMainForm.RemoveNode(Sender: TObject);
begin
  fDetailedProxyForm.Hide;
  ContentTree.DeleteSelectedNodes;
end;

procedure TMainForm.ChangeModbusStatusToolButtonClick(Sender: TObject);
var
  Node: PVirtualNode;
  P: Pointer;
begin
  Screen.Cursor := crHourGLass;
  try

    Node := ContentTree.GetParentNode(TTypeNode.tnModbus, ContentTree.GetFirstSelected);
    if Node <> nil then
    begin
      P := ContentTree.GetNodeData(Node);
      if TContentData(P^) is TModbusData then
      begin
        case TModbusData(P^).Controller.IsOpen of
          False:
          begin
            TModbusData(P^).Controller.Open;
          end;
          True: TModbusData(P^).Controller.Close;
        end;
        ChangeModbusStatusStyle(TModbusData(P^).Controller);
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.SaveHoldings(const aTypeOperation: TTypeOperation);
var
  P: Pointer;
  Node: PVirtualNode;
  DeviceData: TDeviceData;
  ModbusData: TModbusData;
  HoldingsForm: THoldingsForm;
begin
  ModbusData := nil;
  DeviceData := nil;
  // Найти указатели ModbusData, DeviceData
  Node := ContentTree.GetFirstSelected;
  while ContentTree.GetNodeLevel(Node) >= 0 do
  begin

    if ContentTree.GetNodeLevel(Node) = 1 then
    begin
      P := ContentTree.GetNodeData(Node);
      if TContentData(P^) is TDeviceData then
        DeviceData := TDeviceData(P^);
    end;

    if ContentTree.GetNodeLevel(Node) = 0 then
    begin
      P := ContentTree.GetNodeData(Node);
      if TContentData(P^) is TModbusData then
        ModbusData := TModbusData(P^);
      Break;
    end;
    Node := Node^.Parent;
  end;

  if (ModbusData = nil) or (DeviceData = nil) then
    Exit;
  HoldingsForm := THoldingsForm.Create(Self);
  try
    try
      if not ModbusData.Controller.IsOpen then
        raise Exception.Create('Включите канал связи');
      HoldingsForm.SaveHoldings(ModbusData, DeviceData, aTypeOperation);
    except
      on E: Exception do
        MessageBox(Handle, PChar(Utf8ToAnsi(E.Message)),
          PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    end;
  finally
    HoldingsForm.Release;
  end;

end;


procedure TMainForm.RestoreHoldingsToolButtonClick(Sender: TObject);
begin
  SaveHoldings(TTypeOperation.toRestore);
end;

procedure TMainForm.SaveHoldingsToolButtonClick(Sender: TObject);
begin
  SaveHoldings(TTypeOperation.toSave);
end;


procedure TMainForm.StartStopServerToolButtonClick(Sender: TObject);
var
  IsActive: boolean;
begin
  IsActive := (fServer <> nil) and fServer.IsActive;
  try
    case IsActive of
      // Запустить сервер
      False:
      begin
        fServer := GetServer(GetSetting.ServerPort, // Слушающий порт
          Handle
          // Обработчик сообщения - принимает новый контроллер, созданный сервером
          );
        if not fServer.Start then
          raise Exception.Create('Ошибка запуска сервера');
      end;
      // Выключить сервер
      True: fServer := nil;
    end;
    // Время для запуска сервера или его остановки
    Sleep(100);
    ChangeServerStatusStyle;
  except
    on E: Exception do
    begin
      MessageBox(Handle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
      fServer := nil;
    end;
  end;
end;

procedure TMainForm.ModbusToolButtonClick(Sender: TObject);
begin
  TContentBuilders.GetBuilders(ContentTree).BuildTcp;
end;

procedure TMainForm.AddModbus(Sender: TObject);
const
  TCP = 'TCP';
begin
  if Sender is TMenuItem then
    // Выбрать контроллер
    if SameText(TMenuItem(Sender).Caption, TCP) then
      TContentBuilders.GetBuilders(ContentTree).BuildTcp
    else
      TContentBuilders.GetBuilders(ContentTree).BuildRtu(TMenuItem(Sender).Caption);
end;

procedure TMainForm.WmCount(var Message: TMessage);
begin
  if Message.WParam = 1 then
  begin
    StatusBar.Info('Связь OK');
    Inc(fSuccess);
  end;
  if Message.LParam = 1 then
  begin
    StatusBar.Info('Устройство не отвечает');
    Inc(fBad);
  end;
  StatusBar.Count(Format('%d : %d', [fSuccess, fBad]));
end;

procedure TMainForm.WmNewClient(var Message: TMessage);
var
  Controller: IController;
begin
  if Message.wParam <> 0 then
  begin
    Controller := IController({%H-}Pointer(Message.wParam));
    if Controller <> nil then
      TContentBuilders.GetBuilders(ContentTree).BuildAcTcp(Controller);
  end;
end;

procedure TMainForm.SettingsMenuItemClick(Sender: TObject);
begin
  // Диалог настройки программы
  with TSettingForm.Create(Application) do
    ShowModal;
end;

procedure TMainForm.SignatureToolButtonClick(Sender: TObject);
var
  Node: PVirtualNode;
  P: Pointer;
begin
  // Запросить сигнатуры выделенного устройства
  Node := ContentTree.GetParentNode(TTypeNode.tnDevice, ContentTree.GetFirstSelected);
  if Node <> nil then
  begin
    P := ContentTree.GetNodeData(Node);
    if TContentData(P^) is TDeviceData then
      TDeviceData(P^).SeekSignature;
  end;
end;

procedure TMainForm.DeviceToolButtonClick(Sender: TObject);
var
  Node: PVirtualNode;
  P: Pointer;
  ModbusData: TModbusData;
begin
  ModbusData := nil;
  Node := ContentTree.GetFirstSelected;

  // Диалог добавления нового устройства
  try
    if Node = nil then
      raise Exception.Create('Не выделен узел связи');
    Node := ContentTree.GetParentNode(TTypeNode.tnModbus, Node);
    P := ContentTree.GetNodeData(Node);

    if TContentData(P^) is TModbusData then
      ModbusData := TModbusData(P^);

    if ModbusData = nil then
      raise Exception.Create('Не выделен узел связи');

    with TDeviceForm.Create(Application) do
      if ShowModal = mrOk then
        ModbusData.AddDevice(SlaveId, Module);

  except
    on E: Exception do
      MessageBox(Handle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;

procedure TMainForm.ReloadMenuItemClick(Sender: TObject);
	procedure DepthFirstSearch(const aParentNode: PVirtualNode);
  var
    Node: PVirtualNode;
  begin
    Node := aParentNode^.FirstChild;
    while Assigned(Node) do
    begin
      if ContentTree.GetData(Node) is TDeviceData then
        TDeviceData(ContentTree.GetData(Node)).Update(nil)
      else
      	DepthFirstSearch(Node);
      Node := Node^.NextSibling;
    end;
  end;

begin
	StatusBar.Info('Перезагрузка конфигуратора');
  CloseLibrary;
  DepthFirstSearch(ContentTree.RootNode^.FirstChild);
	StatusBar.Info('Конфигуратор обновлен');

end;

{$ENDREGION MainFrom}

end.
