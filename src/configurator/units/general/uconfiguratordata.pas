unit uConfiguratorData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Forms,
  uMap,
  uSignature,
  uModbus,
  uAutoScan,
  uLibrary;

type


  { TDetailedForm }
  TDetailedFormClass = class of TDetailedForm;
  TContentData = class;
  TDetailedForm = class(TForm)
  public
    procedure Load(const aContentData: array of TContentData); virtual; abstract;
    procedure Unload; virtual; abstract;
  end;



  // Базовый класс данных для узлов ContentTree

  { TContentData }

  TContentData = class
    type
    // Типы иконок
    TTypeImage = (iiRtu, iiTcp, iiServer, iiDevice, iiUnknownDevice,
      iiConfig, iiInactive, iiActive, iiBootloader, iiTrends, iiBackup);
    TContentSet = array of TContentData;
  protected
    // Классовая ссылка на форму детализации
    fFormClass: TDetailedFormClass;
    // Указатель на узел, содержащий данный объект
    fOwnerNode: PVirtualNode;
    // Указатель на компонент
    fTree:      TBaseVirtualTree;
  protected
    function GetCaption: string; virtual; abstract;
    function GetImage: TTypeImage; virtual; abstract;
    function GetSelected: TTypeImage; virtual; abstract;
    function GetContentSet: TContentSet; virtual;

  public
    constructor Create;

  public
    // Заголовок узла
    property Caption: string read GetCaption;
    // Иконка невыделенного узла
    property Image: TTypeImage read GetImage;
    // Иконка выделенного узла
    property Selected: TTypeImage read GetSelected;
    // Классовая ссылка на форму детализации
    property FormClass: TDetailedFormClass read fFormClass;

    // Указатель на узел, содержащий данный объект
    property OwnerNode: PVirtualNode read fOwnerNode write fOwnerNode;
    // Указатель на компонент
    property Tree: TBaseVirtualTree read fTree write fTree;
    // Набор объектов типа TContentData для фрм детализации
    property ContentSet: TContentSet read GetContentSet;
  end;

  // Класс, содержащий канал связи с подключенными устройствами

  { TModbusData }
  TModbusData = class(TContentData)
  private
    // Указатель на интерфейс Modbus-контроллера
    fController: IController;
    // Указатель на объект, сканирующий подключенные устройства
    fAutoScan:   TAutoScan;

  protected
    function GetCaption: string; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
    function GetContentSet: TContentSet; override;
  public
    constructor Create(const aController: IController); reintroduce;
    procedure AfterConstruction; override;
    destructor Destroy; override;

  public
    procedure AddDevice(const aSlaveId: byte; const aModule: IModule);
    property AutoScan: TAutoScan read fAutoScan;
    property Controller: IController read fController write fController;
  end;

  { TDeviceData }

  TDeviceData = class(TContentData)
  private
    fSlaveId: byte;
    fMap:     uMap.IMap;
    fModule:  IModule;
    fSignature: ISignature;

  private
    procedure AddSections;

  protected
    procedure SetSlaveId(const aSlaveId: byte);
    procedure SetModule(const aModule: IModule);

    function GetCaption: string; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
    procedure Update(Sender: TObject);
    function GetContentSet: TContentSet; override;
  public
    constructor Create(const aSlaveId: byte); reintroduce;

  public
    procedure SeekSignature;

    property SlaveId: byte read fSlaveId write SetSlaveId;
    property Module: IModule read fModule write SetModule;
    property Map: uMap.IMap read fMap write fMap;
    property Signarute: ISignature read fSignature;
  end;

  { TConfigurationData }

  TConfigurationData = class(TContentData)
  protected
    function GetCaption: string; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  end;


  { TGroupsData }

  TGroupsData = class(TContentData)
  private
    fGroups: IGroups;

  protected
    function GetCaption: string; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
    function GetContentSet: TContentSet; override;
  public
    constructor Create(const aGroups: IGroups); reintroduce;

  public
    property Groups: IGroups read fGroups write fGroups;
  end;

    { TBootloaderData }

    TBootloaderData = class(TContentData)
    protected
      function GetCaption: String; override;
      function GetImage: TTypeImage; override;
      function GetSelected: TTypeImage; override;
      function GetContentSet: TContentSet; override;
    public
      constructor Create;
    end;

    { TTrendsData }

    TTrendsData = class(TContentData)
    protected
      function GetCaption: String; override;
      function GetImage: TTypeImage; override;
      function GetSelected: TTypeImage; override;
      function GetContentSet: TContentSet; override;
    public
      constructor Create;
    end;

    { TBackupData }

    TBackupData = class(TContentData)
    protected
      function GetCaption: String; override;
      function GetImage: TTypeImage; override;
      function GetSelected: TTypeImage; override;
      function GetContentSet: TContentSet; override;
    public
      constructor Create;
    end;



implementation

uses
  uSetting,
  uContentBuilder,
  udetailedgroupform,
  uDetailedModbusForm,
  uDetailedDeviceForm,
  uDetailedBackupForm,
  udetailedtrendsform,
  uDetailedBootloaderForm;


{ TContentData }

function TContentData.GetContentSet: TContentSet;
begin
  Result := nil;
end;

constructor TContentData.Create;
begin
  inherited Create;
  fFormClass := nil;
end;


{ TModbusData }
constructor TModbusData.Create(const aController: IController);
begin
  inherited Create;
  fController := aController;
  fFormClass := TDetailedModbusForm;
end;

procedure TModbusData.AfterConstruction;
begin
  inherited AfterConstruction;
  fAutoScan := TAutoScan.Create(Self);
end;

destructor TModbusData.Destroy;
begin
  FreeAndNil(fAutoScan);
  inherited Destroy;
end;

procedure TModbusData.AddDevice(const aSlaveId: byte; const aModule: IModule);
begin
  TContentBuilders.GetBuilders(fTree).BuildDevice(fOwnerNode, aSlaveId, aModule);
end;

function TModbusData.GetCaption: string;
begin
  Result := fController.ToString;
end;

function TModbusData.GetImage: TTypeImage;
begin
  case fController.TypeController of
    mbRtu: Result   := iiRtu;
    mbAcTcp: Result := iiServer;
    mbCnTcp: Result := iiTcp;
  end;
end;

function TModbusData.GetSelected: TTypeImage;
begin
  case fController.TypeController of
    mbRtu: Result   := iiRtu;
    mbAcTcp: Result := iiServer;
    mbCnTcp: Result := iiTcp;
  end;
end;

function TModbusData.GetContentSet: TContentSet;
begin
  SetLength(Result, 1);
  Result[0] := Self;
end;


{ TDeviceData }
constructor TDeviceData.Create(const aSlaveId: byte);
begin
  inherited Create;
  fSlaveId := aSlaveId;
  fMap     := GetMap;
  fFormClass := TDetailedDeviceForm;
end;

procedure TDeviceData.AddSections;

  procedure AddConfigs(const aNode: PVirtualNode; const aConfigs: IGroups);
  var
    Groups: IGroups;
    Node:    PVirtualNode;
    ContentData: TContentData;
   begin
    for Groups in aConfigs.GroupsList do
    begin
      ContentData := TGroupsData.Create(Groups);
      Node := fTree.AddChild(aNode, ContentData);
      ContentData.Tree := fTree;
      ContentData.OwnerNode := Node;

      AddConfigs(Node, Groups);
    end;
  end;

var
  ContentData: TContentData;
  Node: PVirtualNode;
begin

  // Конфигурации
  ContentData := TConfigurationData.Create;
  Node := fTree.AddChild(fOwnerNode, ContentData);
  ContentData.Tree := fTree;
  ContentData.OwnerNode := Node;
  AddConfigs(Node, fModule.ModuleDefine.Configuration);

  // Прошивка
  ContentData := TBootloaderData.Create;
  Node := fTree.AddChild(fOwnerNode, ContentData);
  ContentData.Tree := fTree;
  ContentData.OwnerNode := Node;

  // Архивация
  //ContentData := TBackupData.Create;
  //Node := fTree.AddChild(fOwnerNode, ContentData);
  //ContentData.Tree := fTree;
  //ContentData.OwnerNode := Node;

  // Тренды
  //ContentData := TTrendsData.Create;
  //Node := fTree.AddChild(fOwnerNode, ContentData);
  //ContentData.Tree := fTree;
  //ContentData.OwnerNode := Node;

  fTree.Expanded[Node^.Parent] := True;

end;

procedure TDeviceData.SetSlaveId(const aSlaveId: byte);
begin
  if fSlaveId = aSlaveId then
    Exit;
  fSlaveId := aSlaveId;
  if (fOwnerNode <> nil) and (fTree <> nil) then
    fTree.InvalidateNode(fOwnerNode);
end;

procedure TDeviceData.SetModule(const aModule: IModule);

begin
  if fModule = aModule then
    Exit;
  fModule := aModule;
  if (fOwnerNode <> nil) and (fTree <> nil) then
    fTree.InvalidateNode(fOwnerNode);

  // Добавить дочерние узлы, вызвав строители
  fTree.DeleteChildren(fOwnerNode);
  if fModule <> nil then
    AddSections;

end;

function TDeviceData.GetCaption: string;
begin
  if fModule = nil then
    Result := Format('%d: неизвестное устройство', [fSlaveId])
  else
    Result := Format('%d: %s', [fSlaveId, fModule.Name]);
end;

function TDeviceData.GetImage: TTypeImage;
begin
  if fModule = nil then
    Result := iiUnknownDevice
  else
    Result := iiDevice;
end;

function TDeviceData.GetSelected: TTypeImage;
begin
  if fModule = nil then
    Result := iiUnknownDevice
  else
    Result := iiDevice;
end;

procedure TDeviceData.Update(Sender: TObject);
var
  FirmWare: word;

  Lib:    ILibrary;
  Sublib: ISublibrary;
  M:      IModule;
  P, P1:  Pointer;

begin

  FirmWare := fSignature.TypeFirmWare;

  // Загрузить библиотеку
  Lib := GetLibrary([GetSetting.DeveloperLibrary + '\module.jlf',
    GetSetting.UserLibrary + '\module.jlf']);

  if Lib = nil then
    Exit;

  for P in Lib do
  begin
    Sublib := Lib.ExtractData(P);
    for P1 in Sublib do
    begin
      M := Sublib.ExtractData(P1);
      if M.Uid = FirmWare then
      begin
        SetModule(M);
        Exit;
      end;
    end;
  end;

end;

function TDeviceData.GetContentSet: TContentSet;
begin
  SetLength(Result, 1);
  Result[0] := Self;
end;

procedure TDeviceData.SeekSignature;
var
  Controller: IController;
  P: Pointer;
  ModbusNode: PVirtualNode;
begin
  {$IFDEF DEBUG}
  Assert(fOwnerNode <> nil);
  Assert(fTree <> nil);
  {$ENDIF}

  ModbusNode := fOwnerNode^.Parent;
  {$IFDEF DEBUG}
  Assert(ModbusNode <> nil);
  {$ENDIF}

  P := fTree.GetNodeData(ModbusNode);
  {$IFDEF DEBUG}
  Assert(TContentData(P^) is TModbusData);
  {$ENDIF}

  Controller := TModbusData(P^).Controller;

  if fModule = nil then
  begin
    fSignature := TAuto.Create(Controller, fMap, fSlaveId, @Update);
    Exit;
  end;
  case fModule.TypeSignature of
    TTypeSignature.sgAuto:
      fSignature := TAuto.Create(Controller, fMap, fSlaveId, @Update);
    TTypeSignature.sgRccu:
      fSignature := TRccu.Create(Controller, fMap, fSlaveId);
    else
      fSignature := TSignature.Create(Controller, fMap, fSlaveId);
  end;
end;

{ TConfigurationData }

function TConfigurationData.GetCaption: string;
begin
  Result := 'Конфигурация';
end;

function TConfigurationData.GetImage: TTypeImage;
begin
  Result := iiConfig;
end;

function TConfigurationData.GetSelected: TTypeImage;
begin
  Result := iiConfig;
end;


{ TGroupsData }
constructor TGroupsData.Create(const aGroups: IGroups);
begin
  inherited Create;
  fGroups := aGroups;
  fFormClass := TDetailedGroupForm;
end;

function TGroupsData.GetCaption: string;
begin
  Result := fGroups.ShortDescription;
end;

function TGroupsData.GetImage: TTypeImage;
begin
  Result := iiInactive;
end;

function TGroupsData.GetSelected: TTypeImage;
begin
  Result := iiActive;
end;

function TGroupsData.GetContentSet: TContentSet;
var
  P: Pointer;
  Node: PVirtualNode;
begin
  SetLength(Result, 3);
  Result[0] := Self;
  Node := fOwnerNode;
  while Node <> nil do
  begin
    if fTree.GetNodeLevel(Node) = 1 then
    begin
      P := fTree.GetNodeData(Node);
      Result[1] := TContentData(P^);
    end;

    if fTree.GetNodeLevel(Node) = 0 then
    begin
      P := fTree.GetNodeData(Node);
      Result[2] := TContentData(P^);
      Break;
    end;
    Node := Node^.Parent;
  end;
end;

{ TBackupData }

constructor TBackupData.Create;
begin
  inherited Create;
  fFormClass := TDetailedBackupForm;
end;

function TBackupData.GetCaption: String;
begin
  Result := 'Архивация';
end;

function TBackupData.GetImage: TTypeImage;
begin
  Result := iiBackup;
end;

function TBackupData.GetSelected: TTypeImage;
begin
  Result := iiBackup;
end;

function TBackupData.GetContentSet: TContentSet;
var
  P: Pointer;
  Node: PVirtualNode;
begin
  SetLength(Result, 3);
  Result[0] := Self;
  Node := fOwnerNode;
  while Node <> nil do
  begin
    if fTree.GetNodeLevel(Node) = 1 then
    begin
      P := fTree.GetNodeData(Node);
      Result[1] := TContentData(P^);
    end;

    if fTree.GetNodeLevel(Node) = 0 then
    begin
      P := fTree.GetNodeData(Node);
      Result[2] := TContentData(P^);
      Break;
    end;
    Node := Node^.Parent;
  end;
end;

{ TTrendsData }

constructor TTrendsData.Create;
begin
  inherited Create;
  fFormClass := TDetailedTrendsForm;
end;

function TTrendsData.GetCaption: String;
begin
  Result := 'Тренды';
end;

function TTrendsData.GetImage: TTypeImage;
begin
  Result := iiTrends;
end;

function TTrendsData.GetSelected: TTypeImage;
begin
  Result := iiTrends;
end;

function TTrendsData.GetContentSet: TContentSet;
var
  P: Pointer;
  Node: PVirtualNode;
begin
  SetLength(Result, 3);
  Result[0] := Self;
  Node := fOwnerNode;
  while Node <> nil do
  begin
    if fTree.GetNodeLevel(Node) = 1 then
    begin
      P := fTree.GetNodeData(Node);
      Result[1] := TContentData(P^);
    end;

    if fTree.GetNodeLevel(Node) = 0 then
    begin
      P := fTree.GetNodeData(Node);
      Result[2] := TContentData(P^);
      Break;
    end;
    Node := Node^.Parent;
  end;
end;

{ TBootloaderData }

function TBootloaderData.GetCaption: String;
begin
  Result := 'Прошивка';
end;

function TBootloaderData.GetImage: TTypeImage;
begin
  Result := iiBootloader;
end;

function TBootloaderData.GetSelected: TTypeImage;
begin
  Result := iiBootloader;
end;

function TBootloaderData.GetContentSet: TContentSet;
var
  P: Pointer;
  Node: PVirtualNode;
begin
  SetLength(Result, 3);
  Result[0] := Self;
  Node := fOwnerNode;
  while Node <> nil do
  begin
    if fTree.GetNodeLevel(Node) = 1 then
    begin
      P := fTree.GetNodeData(Node);
      Result[1] := TContentData(P^);
    end;

    if fTree.GetNodeLevel(Node) = 0 then
    begin
      P := fTree.GetNodeData(Node);
      Result[2] := TContentData(P^);
      Break;
    end;
    Node := Node^.Parent;
  end;
end;

constructor TBootloaderData.Create;
begin
  inherited Create;
  fFormClass := TDetailedBootloaderForm;
end;
end.




