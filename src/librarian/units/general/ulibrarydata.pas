unit uLibraryData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Forms,
  uLibrary;

type

  TEditingFormClass = class of TEditingForm;

  { TLibraryData }

  TLibraryData = class
  protected
    fOwnerNode: PVirtualNode;
    fTree: TBaseVirtualTree;

  protected
    function GetCaption: string; virtual; abstract;
    function GetFormClass: TEditingFormClass; virtual;
    function GetImageIndex: integer; virtual;

  public
    property Caption: string read GetCaption;
    property ImageIndex: integer read GetImageIndex;
    property FormClass: TEditingFormClass read GetFormClass;
    property OwnerNode: PVirtualNode read fOwnerNode write fOwnerNode;
    property Tree: TBaseVirtualTree read fTree write fTree;
  end;

  { TEditingForm }

  TEditingForm = class(TForm)
  public
    procedure Load(const aLibraryData: TLibraryData); virtual; abstract;
    procedure Unload; virtual; abstract;
  end;


  { TSubLibraryData }

  TSubLibraryData = class(TLibraryData)
  private
    fSublibrary: ISublibrary;
    fTypeSublibrary: TTypeSublibrary;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;

  public
    constructor Create(const aTypeSublibrary: TTypeSublibrary;
      const aSublibrary: ISublibrary);
  public
    property Sublibrary: ISublibrary read fSublibrary;
    property TypeSublibrary: TTypeSublibrary read fTypeSublibrary;
  end;


  { TModuleData }

  TModuleData = class(TLibraryData)
  private
    fModule: IModule;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;
    function GetFormClass: TEditingFormClass; override;

  public
    constructor Create(const aModule: IModule);
  public
    property Module: IModule read fModule;
  end;

  { TBaseDescData }

  TBaseDescData = class(TLibraryData)
  private
    fBaseDesc: IBaseDescription;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;
    function GetFormClass: TEditingFormClass; override;

  public
    constructor Create(const aBaseDesc: IBaseDescription);
  public
    property BaseDesc: IBaseDescription read fBaseDesc;
  end;

  { TPresetsData }

  TPresetsData = class(TLibraryData)
  private
    fPresets: IPresets;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;
  public
    constructor Create(const aPresets: IPresets);
  public
    property Presets: IPresets read fPresets;
  end;


  { TPickListsData }

  TPickListsData = class(TLibraryData)
  private
    fPickLists: IPicklists;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;
    function GetFormClass: TEditingFormClass; override;
  public
    constructor Create(const aPickLists: IPickLists);
  public
    property PickLists: IPickLists read fPickLists;
  end;


  { TBitsData }

  TBitsData = class(TLibraryData)
  private
    fBitsSet: IBitsSet;
  protected
    function GetCaption: string; override;
    function GetImageIndex: integer; override;
    function GetFormClass: TEditingFormClass; override;
  public
    constructor Create(const aBitsSet: IBitsSet);
  public
    property BitsSet: IBitsSet read fBitsSet;
  end;

 { TRegistersData }

 TRegistersData = class(TLibraryData)
 private
   fRegisters: IRegisters;
 protected
   function GetCaption: string; override;
   function GetImageIndex: integer; override;
 public
   constructor Create(const aRegisters: IRegisters);
 end;

 { TVarsData }

 TVarsData = class(TLibraryData)
 private
   fVars: IVars;
   fTypeRegister: TTypeRegister;
 protected
   function GetCaption: string; override;
   function GetImageIndex: integer; override;
   function GetFormClass: TEditingFormClass; override;
 public
   constructor Create(const aTypeRegister: TTypeRegister; const aVars: IVars); reintroduce;
 public
   property Vars: IVars read fVars;
   property TypeRegister: TtYpeRegister read fTypeRegister;
 end;

 { TConfigurationData }

 TConfigurationData = class(TLibraryData)
 private
   fConfiguration: IGroups;
   fRegisters: IRegisters;
 protected
   function GetCaption: string; override;
   function GetImageIndex: integer; override;
   function GetFormClass: TEditingFormClass; override;
 public
   constructor Create(const aRegisters: IRegisters; const aConfiguration: IGroups); reintroduce;
 public
   property Configuration: IGroups read fConfiguration;
   property Registers: IRegisters read fRegisters;
 end;


implementation

uses
  uBitsForm,
  uPickListForm,
  uVarsForm,
  uBaseDescForm,
  uModuleForm,
  uEmptyForm,
  uConfigForm;

{ TConfigurationData }

function TConfigurationData.GetCaption: string;
begin
	Result := 'Конфигурация';
end;

function TConfigurationData.GetImageIndex: integer;
begin
  Result:= 9;
end;

function TConfigurationData.GetFormClass: TEditingFormClass;
begin
  Result := TConfigForm;
end;

constructor TConfigurationData.Create(const aRegisters: IRegisters;
  const aConfiguration: IGroups);
begin
	inherited Create;
  fRegisters := aRegisters;
  fConfiguration := aConfiguration;
end;

{ TVarsData }

function TVarsData.GetCaption: string;
begin
  case fTypeRegister of
	  trHolding: Result := 'Input';
  	trInput: Result := 'Holding';
  end;
end;

function TVarsData.GetImageIndex: integer;
begin
  case fTypeRegister of
	  trHolding: Result := 7;
  	trInput: Result := 8;
  end;
end;

function TVarsData.GetFormClass: TEditingFormClass;
begin
  Result := TVarsForm;
end;

constructor TVarsData.Create(const aTypeRegister: TTypeRegister;
  const aVars: IVars);
begin
	inherited Create;
  fTypeRegister := aTypeRegister;
  fVars := aVars;
end;

{ TRegistersData }

function TRegistersData.GetCaption: string;
begin
	Result := 'Регистры';
end;

function TRegistersData.GetImageIndex: integer;
begin
  Result := 6;
end;

constructor TRegistersData.Create(const aRegisters: IRegisters);
begin
 	inherited Create;
  fRegisters := aRegisters;
end;

{ TBitsData }

function TBitsData.GetCaption: string;
begin
  Result := 'Набор битов';
end;

function TBitsData.GetImageIndex: integer;
begin
  Result := 5;
end;

function TBitsData.GetFormClass: TEditingFormClass;
begin
  Result := TBitsForm;
end;

constructor TBitsData.Create(const aBitsSet: IBitsSet);
begin
  inherited Create;
  fBitsSet := aBitsSet;
end;

{ TPickListsData }

function TPickListsData.GetCaption: string;
begin
  Result := 'Списки';
end;

function TPickListsData.GetImageIndex: integer;
begin
  Result := 5;
end;

function TPickListsData.GetFormClass: TEditingFormClass;
begin
  Result := TPickListForm;
end;

constructor TPickListsData.Create(const aPickLists: IPickLists);
begin
  inherited Create;
  fPickLists := aPickLists;
end;

{ TPresetsData }

function TPresetsData.GetCaption: string;
begin
  Result := 'Предустановленные значения';
end;

function TPresetsData.GetImageIndex: integer;
begin
  Result := 4;
end;

constructor TPresetsData.Create(const aPresets: IPresets);
begin
  inherited Create;
  fPresets := aPresets;
end;

{ TBaseDescData }

function TBaseDescData.GetCaption: string;
begin
  Result := 'Описание модуля';
end;

function TBaseDescData.GetImageIndex: integer;
begin
  Result := 3;
end;

function TBaseDescData.GetFormClass: TEditingFormClass;
begin
  Result := TBaseDescForm;
end;

constructor TBaseDescData.Create(const aBaseDesc: IBaseDescription);
begin
  inherited Create;
  fBasedesc := aBasedesc;
end;

{ TModuleData }

constructor TModuleData.Create(const aModule: IModule);
begin
  inherited Create;
  fModule := aModule;
end;

function TModuleData.GetCaption: string;
begin
  Result := fModule.Name;
end;

function TModuleData.GetImageIndex: integer;
begin
  Result := 2;
end;

function TModuleData.GetFormClass: TEditingFormClass;
begin
  Result := TModuleForm;
end;


{ TSubLibraryData }

function TSubLibraryData.GetCaption: string;
begin
  case fTypeSublibrary of
    slDeveloper: Result := 'Библиотека разработчика';
    slUser: Result := 'Библиотека пользователя';
  end;
end;

function TSubLibraryData.GetImageIndex: integer;
begin
  case fTypeSublibrary of
    slDeveloper: Result := 0;
    slUser: Result := 1;
  end;
end;

constructor TSubLibraryData.Create(const aTypeSublibrary: TTypeSublibrary;
  const aSublibrary: ISublibrary);
begin
  inherited Create;
  fSublibrary := aSublibrary;
  fTypeSublibrary := aTypeSublibrary;
end;


{ TLibraryData }

function TLibraryData.GetImageIndex: integer;
begin
  Result := -1;
end;

function TLibraryData.GetFormClass: TEditingFormClass;
begin
  Result := TEmptyForm;
end;

end.
