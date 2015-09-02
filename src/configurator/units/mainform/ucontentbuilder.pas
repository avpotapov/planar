unit uContentBuilder;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, VirtualTrees, Windows, Forms,
  fgl,
  uLibrary,
  uModbus,
  uConfiguratorData;

type

  { IContentBuilder }

  IContentBuilder = interface
    ['{F114DCE5-D31F-4607-AE67-77F72031D7A4}']
    procedure AfterBuild;
    procedure BuildContentData;
    procedure BuildNode;
    procedure BeforeBuild;
    procedure Build;

    function GetNode: PVirtualNode;
    function GetContentData: TContentData;

    property Node: PVirtualNode read GetNode;
    property ContentData: TContentData read GetContentData;
  end;

  { TContentBuilder }

  TContentBuilder = class(TInterfacedObject, IContentBuilder)
  protected
    fTree:   TBaseVirtualTree;
    fNode:   PVirtualNode;
    fParent: PVirtualNode;
    fContentData: TContentData;

  protected
    procedure AfterBuild; virtual;
    procedure BuildContentData; virtual;
    procedure BuildNode; virtual;
    procedure BeforeBuild; virtual;
    procedure Build;

    function GetNode: PVirtualNode;
    function GetContentData: TContentData;
  public
    constructor Create(const aTree: TBaseVirtualTree); reintroduce;

  end;

  { TModbusBuilder }

  TModbusBuilder = class(TContentBuilder)
  protected
    fController: IController;

  protected
    // Найти дубликат связи по имени в дереве
    procedure FindDuplicates(const aModbusName: string);

    procedure BuildContentData; override;
    procedure BuildNode; override;
  end;

  { TModbusTcpBuilder }
  TModbusTcpBuilder = class(TModbusBuilder)
  private
    fIp:   string;
    fPort: word;
  protected
    procedure BeforeBuild; override;
    procedure BuildContentData; override;
  end;

  { TModbusRtuBuilder }
  IModbusRtuBuilder = interface(IContentBuilder)
    ['{5509F990-74FB-487C-AE20-B64586F33F01}']
    procedure SetPortName(const aPortName: string);
    property PortName: string write SetPortName;
  end;

  TModbusRtuBuilder = class(TModbusBuilder, IModbusRtuBuilder)
  private
    fPortName: string;
  protected
    procedure SetPortName(const aPortName: string);
    procedure BeforeBuild; override;
    procedure BuildContentData; override;
  end;

  { TModbusAcTcpBuilder }
  IModbusAcTcpBuilder = interface(IContentBuilder)
    ['{5509F990-74FB-487C-AE20-B64586F33F01}']
    procedure SetController(const aController: IController);
    property Controller: IController write SetController;
  end;

  TModbusAcTcpBuilder = class(TModbusBuilder, IModbusAcTcpBuilder)
  protected
    procedure SetController(const aController: IController);
  end;


  { TDeviceBuilder }

  IDeviceBuilder = interface(IContentBuilder)
    ['{A113ACF2-ED95-4EF5-8CDE-B17320B69AD6}']
    procedure SetDevice(const aParent: PVirtualNode;
      const aSlaveId: Byte; const aModule: IModule = nil);
  end;

  TDeviceBuilder = class(TContentBuilder, IDeviceBuilder)
  private
    fSlaveId: byte;
    fModule: IModule;
   protected
    // Найти дубликат связи по имени в дереве
    procedure FindDuplicates(const aSlaveId: Byte);

    procedure BeforeBuild; override;
    procedure BuildContentData; override;
    procedure BuildNode; override;
    procedure AfterBuild; override;

    procedure SetDevice(const aParent: PVirtualNode;
      const aSlaveId: Byte; const aModule: IModule = nil);
  end;

  { TContentBuilders }

  TContentBuilders = class
    type
    TTypeBuilder = (cbRtu, cbTcp, cbAcTcp, cbDevice, cbConfigs);
    TBuilderPool = specialize TFpgMap<TTypeBuilder, IContentBuilder>;
  private
    class var  fInstance: TContentBuilders;
    fBuilders: TBuilderPool;
    fTree: TBaseVirtualTree;

  private
    {%H-}constructor Create(const aTree: TBaseVirtualTree); reintroduce;
    {%H-}destructor {%H-}Destroy; override;

  public
    class function GetBuilders(const aTree: TBaseVirtualTree):
      TContentBuilders;

  public
    procedure BuildRtu(const aPortName: string);
    procedure BuildTcp;
    procedure BuildAcTcp(const aController: IController);
    procedure BuildDevice(const aParent: PVirtualNode; const aSlaveId: byte;
      const aModule: IModule = nil);
  end;

implementation

uses
  uSetting;

{ TModbusAcTcpBuilder }

procedure TModbusAcTcpBuilder.SetController(const aController: IController);
begin
  fController := aController;
end;

{ TContentBuilder }
constructor TContentBuilder.Create(const aTree: TBaseVirtualTree);
begin
  inherited Create;
  fTree := aTree;
end;

procedure TContentBuilder.Build;
begin
  {$IFDEF DEBUG}
  Assert(fTree <> nil);
  {$ENDIF}

  fNode := nil;
  fContentData := nil;

  BeforeBuild;

  if (fNode = nil) and (fContentData = nil) then
  begin
    BuildContentData;
    BuildNode;
    AfterBuild;
  end;

end;

function TContentBuilder.GetNode: PVirtualNode;
begin
  Result := fNode;
end;

function TContentBuilder.GetContentData: TContentData;
begin
  Result := fContentData;
end;


procedure TContentBuilder.BeforeBuild;
begin
  { TODO : Stub }
end;

procedure TContentBuilder.BuildContentData;
begin
  { TODO : Stub }
end;

procedure TContentBuilder.BuildNode;
begin
  {$IFDEF DEBUG}
  Assert(fContentData <> nil);
  {$ENDIF}

  fNode := fTree.AddChild(fParent, fContentData);
end;

procedure TContentBuilder.AfterBuild;
begin
  {$IFDEF DEBUG}
  Assert(fNode <> nil);
  {$ENDIF}

  // Добавить указатели на визуальные объекты в данные
  if fContentData <> nil then
  begin
    fContentData.Tree      := fTree;
    fContentData.OwnerNode := fNode;
  end;
end;


{ TModbusBuilder }

procedure TModbusBuilder.FindDuplicates(const aModbusName: string);
var
  P: Pointer;
begin
  fNode := fTree.GetFirstChild(fTree.RootNode);
  while fNode <> nil do
  begin
    P := fTree.GetNodeData(fNode);
    if TContentData(P^).Caption = aModbusName then
    begin
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Найден дубликат узла связи')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);

      fTree.SetFocus;
      fTree.Selected[fNode] := True;
      Exit;
    end;
    fNode := fTree.GetNextSibling(fNode);
  end;
end;

procedure TModbusBuilder.BuildContentData;
begin
  inherited BuildContentData;
  {$IFDEF DEBUG}
  Assert(fController <> nil);
  {$ENDIF}
  fContentData := TModbusData.Create(fController);
end;

procedure TModbusBuilder.BuildNode;
begin
  inherited BuildNode;
  fTree.Selected[fNode] := True;
end;

{ TModbusTcpBuilder }

procedure TModbusTcpBuilder.BeforeBuild;
begin
  inherited BeforeBuild;

  fIp   := GetSetting.Ip;
  fPort := GetSetting.Port;

  {$IFDEF DEBUG}
  Assert(fIp <> '');
  Assert(fPort > 0);
  {$ENDIF}

  // Если найден дубликат узла, то fNode <> nil
  FindDuplicates(Format('%s: %d', [fIp, fPort]));

end;

procedure TModbusTcpBuilder.BuildContentData;
begin
  // Новый TCP-контроллер
  fController := GetTcpBuilder.GetController(fIp, fPort);

  inherited BuildContentData;
end;

{ TModbusRtuBuilder }

procedure TModbusRtuBuilder.SetPortName(const aPortName: string);
begin
  fPortName := aPortName;
end;

procedure TModbusRtuBuilder.BeforeBuild;
begin
  inherited BeforeBuild;

  {$IFDEF DEBUG}
  Assert(fPortName <> '');
  {$ENDIF}

  // Если найден дубликат узла, то fNode <> nil
  FindDuplicates(Format('%s', [fPortName]));
end;

procedure TModbusRtuBuilder.BuildContentData;
begin
  // Новый RTU-контроллер
  fController := GetRtuBuilder.GetController(PChar(fPortName),
    GetSetting.BaudRate, GetSetting.Parity, GetSetting.StopBits,
    GetSetting.ByteSize, GetSetting.ThreeAndHalf);

  inherited BuildContentData;
end;

{ TDeviceBuilder }

procedure TDeviceBuilder.FindDuplicates(const aSlaveId: Byte);
var
  P: Pointer;
begin
  {$IFDEF DEBUG}
  Assert(fParent <> nil);
  {$ENDIF}

  fNode := fTree.GetFirstChild(fParent);
  while fNode <> nil do
  begin
    P := fTree.GetNodeData(fNode);
    if (TContentData(P^) is TDeviceData) and (TDeviceData(P^).SlaveId = aSlaveId) then
    begin

      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Найден дубликат устройства')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);

      fTree.SetFocus;
      fTree.Selected[fNode] := True;
      Exit;
    end;
    fNode := fTree.GetNextSibling(fNode);
  end;
end;

procedure TDeviceBuilder.BeforeBuild;
begin
  inherited BeforeBuild;

  {$IFDEF DEBUG}
  Assert(fSlaveId > 0);
  {$ENDIF}

  // Если найден дубликат узла, то fNode <> nil
  FindDuplicates(fSlaveId);

end;

procedure TDeviceBuilder.BuildContentData;
begin
  inherited BuildContentData;
  fContentData := TDeviceData.Create(fSlaveId);
end;

procedure TDeviceBuilder.BuildNode;
begin
  inherited BuildNode;
  fTree.Selected[fNode] := True;
  fTree.Expanded[fNode^.Parent] := True;
end;

procedure TDeviceBuilder.AfterBuild;
begin
  inherited AfterBuild;
  if (fContentData <> nil) and (fContentData is TDeviceData) then
    TDeviceData(fContentData).Module := fModule;
  // Поиск устройства
  TDeviceData(fContentData).SeekSignature;
end;

procedure TDeviceBuilder.SetDevice(const aParent: PVirtualNode;
  const aSlaveId: Byte; const aModule: IModule);
begin
  fParent := aParent;
  fSlaveId := aSlaveId;
  fModule := aModule;
end;

{ TContentBuilders }

constructor TContentBuilders.Create(const aTree: TBaseVirtualTree);
begin
  inherited Create;
  fTree := aTree;
  fBuilders := TBuilderPool.Create;
end;

class function TContentBuilders.GetBuilders(
  const aTree: TBaseVirtualTree): TContentBuilders;
begin
  if fInstance = nil then
    fInstance := TContentBuilders.Create(aTree);
  Result      := fInstance;
end;

destructor TContentBuilders.Destroy;
begin
  FreeAndNil(fBuilders);
  inherited Destroy;
end;

procedure TContentBuilders.BuildRtu(const aPortName: string);
var
  FoundIndex: integer;
  ModbusRtuBuilder: IModbusRtuBuilder;
begin
  FoundIndex := fBuilders.IndexOf(TTypeBuilder.cbRtu);

  if FoundIndex < 0 then
    FoundIndex := fBuilders.Add(TTypeBuilder.cbRtu, TModbusRtuBuilder.Create(fTree));

  if Supports(fBuilders.Data[FoundIndex], IModbusRtuBuilder, ModbusRtuBuilder) then
  begin
    ModbusRtuBuilder.PortName := aPortName;
    ModbusRtuBuilder.Build;
  end;

end;

procedure TContentBuilders.BuildTcp;
var
  FoundIndex: integer;
begin
  FoundIndex := fBuilders.IndexOf(TTypeBuilder.cbTcp);

  if FoundIndex < 0 then
    FoundIndex := fBuilders.Add(TTypeBuilder.cbTcp, TModbusTcpBuilder.Create(fTree));

  fBuilders.Data[FoundIndex].Build;

end;

procedure TContentBuilders.BuildAcTcp(const aController: IController);
var
  FoundIndex: integer;
  ModbusAcTcpBuilder: IModbusAcTcpBuilder;
begin
  FoundIndex := fBuilders.IndexOf(TTypeBuilder.cbAcTcp);

  if FoundIndex < 0 then
    FoundIndex := fBuilders.Add(TTypeBuilder.cbAcTcp,
      TModbusAcTcpBuilder.Create(fTree));

  if Supports(fBuilders.Data[FoundIndex], IModbusAcTcpBuilder,
    ModbusAcTcpBuilder) then
  begin
    ModbusAcTcpBuilder.Controller := aController;
    ModbusAcTcpBuilder.Build;
  end;

end;

procedure TContentBuilders.BuildDevice(const aParent: PVirtualNode;
  const aSlaveId: byte; const aModule: IModule);
var
  FoundIndex: integer;
  DeviceBuilder: IDeviceBuilder;
begin
  FoundIndex := fBuilders.IndexOf(TTypeBuilder.cbDevice);

  if FoundIndex < 0 then
    FoundIndex := fBuilders.Add(TTypeBuilder.cbDevice,
      TDeviceBuilder.Create(fTree));

  if Supports(fBuilders.Data[FoundIndex], IDeviceBuilder,
    DeviceBuilder) then
  begin
    DeviceBuilder.SetDevice(aParent, aSlaveId, aModule);
    DeviceBuilder.Build;
  end;
end;





initialization

finalization
  FreeAndNil(TContentBuilders.fInstance);

end.
