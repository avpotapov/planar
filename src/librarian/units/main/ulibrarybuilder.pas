unit uLibraryBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees,
  uLibrary,
  uLibraryData;

type
  ILibraryBuilder = interface
    ['{0248C43C-1EAC-480E-AF4A-34BB61A2B11C}']
    procedure BuildData;
    procedure BuildNode;
    procedure AfterBuild;

    procedure SetParentNode(const aNode: PVirtualNode);
    procedure SetTree(const aTree: TBaseVirtualTree);

    property ParentNode: PVirtualNode write SetParentNode;
    property Tree: TBaseVirtualTree write SetTree;
  end;

  { TLibraryBuilder }

  TLibraryBuilder = class(TInterfacedObject, ILibraryBuilder)
  protected
    fParentNode: PVirtualNode;
    fOwnerNode: PVirtualNode;
    fTree: TBaseVirtualTree;
    fLibraryData: TLibraryData;

  protected
    procedure BuildData; virtual; abstract;
    procedure BuildNode; virtual;
    procedure AfterBuild; virtual;

    procedure SetParentNode(const aNode: PVirtualNode);
    procedure SetTree(const aTree: TBaseVirtualTree);

  public
    constructor Create;

    property ParentNode: PVirtualNode read fParentNode write SetParentNode;
    property Tree: TBaseVirtualTree read fTree write SetTree;

  end;

  { TLibraryDirector }

  TLibraryDirector = class
  public
    class procedure Build(const aLibraryBuilder: ILibraryBuilder);
  end;

  { TSublibraryBuilder }

  TSublibraryBuilder = class(TLibraryBuilder)
  private
    fTypeSublibrary: TTypeSublibrary;
    fSublibrary: ISublibrary;
  public
    procedure AfterBuild; override;
    procedure BuildData; override;
    constructor Create(const aTypeSublibrary: TTypeSublibrary;
      const aSublibrary: ISublibrary);
  end;


  { TModuleBuilder }

  TModuleBuilder = class(TLibraryBuilder)
  private
    fModule: IModule;
  public
    procedure AfterBuild; override;
    procedure BuildData; override;
    constructor Create(const aModule: IModule);
  end;


  { TBaseDescBuilder }

  TBaseDescBuilder = class(TLibraryBuilder)
  private
    fBaseDesc: IBaseDescription;
  public
    procedure BuildData; override;
    constructor Create(const aBasedesc: IBaseDescription);
  end;

  { TPreSetsBuilder }

  TPreSetsBuilder = class(TLibraryBuilder)
  private
    fPresets: IPresets;
  public
    procedure AfterBuild; override;
    procedure BuildData; override;
    constructor Create(const aPresets: IPresets);
  end;


  { TPickListsBuilder }

  TPickListsBuilder = class(TLIbraryBuilder)
  private
    fPickLists: IPickLists;
  public
    procedure BuildData; override;
    constructor Create(const aPickLists: IPickLists);
  end;


  { TBitsBuilder }

  TBitsBuilder = class(TLIbraryBuilder)
  private
    fBitsSet: IBitsSet;
  public
    procedure BuildData; override;
    constructor Create(const aBitsSet: IBitsSet);
  end;

  { TRegistersBuilder }

  TRegistersBuilder = class(TLibraryBuilder)
  private
    fRegisters: IRegisters;
  public
    procedure AfterBuild; override;
    procedure BuildData; override;
    constructor Create(const aRegisters: IRegisters);
  end;

  { TVarsBuilder }

  TVarsBuilder = class(TLibraryBuilder)
  private
    fVars: IVars;
    fTypeRegister: TTypeRegister;
  public
    procedure BuildData; override;
    constructor Create(const aTypeRegister: TTypeRegister; const aVars: IVars);
  end;

  { TConfigurationBuilder }

  TConfigurationBuilder = class(TLIbraryBuilder)
  private
    fConfiguration: IGroups;
    fRegisters: IRegisters;
  public
    procedure BuildData; override;
    constructor Create(const aRegisters: IRegisters; const aConfiguration: IGroups);
  end;

implementation

{ TConfigurationBuilder }

procedure TConfigurationBuilder.BuildData;
begin
	fLibraryData := TConfigurationData.Create(fRegisters, fConfiguration);
end;

constructor TConfigurationBuilder.Create(const aRegisters: IRegisters;
  const aConfiguration: IGroups);
begin
	inherited Create;
  fRegisters := aRegisters;
  fConfiguration := aConfiguration;
end;

{ TVarsBuilder }


procedure TVarsBuilder.BuildData;
begin
	fLibraryData := TVarsData.Create(fTypeRegister, fVars);
end;

constructor TVarsBuilder.Create(const aTypeRegister: TTypeRegister;
  const aVars: IVars);
begin
	inherited Create;
  fTypeRegister :=  aTypeRegister;
  fVars := aVars;
end;

{ TRegistersBuilder }

procedure TRegistersBuilder.AfterBuild;
var
  P: Pointer;
  Builder: ILibraryBuilder;
begin
  inherited AfterBuild;

  // Инициализировать Holding, Input
  if fRegisters.IndexOf(TTypeRegister.trHolding) < 0 then
    fRegisters.Add(TTypeRegister.trHolding);
  if fRegisters.IndexOf(TTypeRegister.trInput) < 0 then
    fRegisters.Add(TTypeRegister.trInput);

  for P in fRegisters do
  begin
  	Builder := TVarsBuilder.Create(fRegisters.ExtractKey(P), fRegisters.ExtractData(P));
    Builder.ParentNode := fOwnerNode;
    Builder.Tree := fTree;
    TLibraryDirector.Build(Builder);
  end;

end;

procedure TRegistersBuilder.BuildData;
begin
	fLibraryData := TRegistersData.Create(fRegisters);
end;

constructor TRegistersBuilder.Create(const aRegisters: IRegisters);
begin
	inherited Create;
  fRegisters := aRegisters;
end;

{ TBitsBuilder }

procedure TBitsBuilder.BuildData;
begin
  fLibraryData := TBitsData.Create(fBitsSet);
end;

constructor TBitsBuilder.Create(const aBitsSet: IBitsSet);
begin
  inherited Create;
  fBitsSet := aBitsSet;
end;

{ TPickListsBuilder }

procedure TPickListsBuilder.BuildData;
begin
  fLibraryData := TPickListsData.Create(fPicklists);
end;

constructor TPickListsBuilder.Create(const aPickLists: IPickLists);
begin
  inherited Create;
  fPickLists := aPickLists;
end;

{ TPreSetsBuilder }

procedure TPreSetsBuilder.AfterBuild;
var
  Builder: ILibraryBuilder;
begin
  inherited AfterBuild;

  Builder := TPickListsBuilder.Create(fPresets.PickLists);
  Builder.ParentNode := fOwnerNode;
  Builder.Tree := fTree;
  TLibraryDirector.Build(Builder);


  Builder := TBitsBuilder.Create(fPresets.BitsSet);
  Builder.ParentNode := fOwnerNode;
  Builder.Tree := fTree;
  TLibraryDirector.Build(Builder);

end;

procedure TPreSetsBuilder.BuildData;
begin
  fLibraryData := TPresetsData.Create(fPresets);
end;

constructor TPreSetsBuilder.Create(const aPresets: IPresets);
begin
  inherited Create;
  fPresets := aPresets;
end;

{ TBaseDescBuilder }

procedure TBaseDescBuilder.BuildData;
begin
  fLibraryData := TBaseDescData.Create(fBaseDesc);
end;

constructor TBaseDescBuilder.Create(const aBasedesc: IBaseDescription);
begin
  inherited Create;
  fBaseDesc := aBaseDesc;
end;

{ TModuleBuilder }
constructor TModuleBuilder.Create(const aModule: IModule);
begin
  inherited Create;
  fModule := aModule;
end;


procedure TModuleBuilder.BuildData;
begin
  fLibraryData := TModuleData.Create(fModule);
end;


procedure TModuleBuilder.AfterBuild;
var
  Builder: ILibraryBuilder;
begin
  inherited AfterBuild;

  Builder := TBaseDescBuilder.Create(fModule.ModuleDefine.BaseDescription);
  Builder.ParentNode := fOwnerNode;
  Builder.Tree := fTree;
  TLibraryDirector.Build(Builder);

  Builder := TPresetsBuilder.Create(fModule.ModuleDefine.PreSets);
  Builder.ParentNode := fOwnerNode;
  Builder.Tree := fTree;
  TLibraryDirector.Build(Builder);

  Builder := TRegistersBuilder.Create(fModule.ModuleDefine.Registers);
  Builder.ParentNode := fOwnerNode;
  Builder.Tree := fTree;
  TLibraryDirector.Build(Builder);

  Builder := TConfigurationBuilder.Create(fModule.ModuleDefine.Registers,
  	fModule.ModuleDefine.Configuration);
  Builder.ParentNode := fOwnerNode;
  Builder.Tree := fTree;
  TLibraryDirector.Build(Builder);
end;



{ TLibraryBuilder }

procedure TLibraryBuilder.BuildNode;
begin
  fOwnerNode := fTree.AddChild(fParentNode, fLibraryData);
end;

procedure TLibraryBuilder.AfterBuild;
begin
  fLibraryData.OwnerNode := fOwnerNode;
  fLibraryData.Tree := fTree;
end;

procedure TLibraryBuilder.SetParentNode(const aNode: PVirtualNode);
begin
  fParentNode := aNode;
end;

procedure TLibraryBuilder.SetTree(const aTree: TBaseVirtualTree);
begin
  fTree := aTree;
end;

constructor TLibraryBuilder.Create;
begin
  inherited Create;
  fParentNode := nil;
end;

{ TSublibraryBuilder }

constructor TSublibraryBuilder.Create(const aTypeSublibrary: TTypeSublibrary;
  const aSublibrary: ISublibrary);
begin
  inherited Create;
  fSublibrary := aSublibrary;
  fTypeSublibrary := aTypeSublibrary;
end;

procedure TSublibraryBuilder.BuildData;
begin
  fLibraryData := TSublibraryData.Create(fTypeSublibrary, fSublibrary);
end;


procedure TSublibraryBuilder.AfterBuild;
var
  Builder: ILibraryBuilder;
  P: Pointer;
begin
  inherited AfterBuild;

  // Список модулей
  for P in fSublibrary do
  begin
    Builder := TModuleBuilder.Create(fSublibrary.ExtractData(P));
    Builder.ParentNode := fOwnerNode;
    Builder.Tree := fTree;
    TLibraryDirector.Build(Builder);
  end;
  fTree.Expanded[fOwnerNode] := True;
end;



{ TLibraryDirector }

class procedure TLibraryDirector.Build(const aLibraryBuilder: ILibraryBuilder);
begin
  aLibraryBuilder.BuildData;
  aLibraryBuilder.BuildNode;
  aLibraryBuilder.AfterBuild;
end;


end.

