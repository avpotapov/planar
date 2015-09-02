unit uLibraryTreeData;

(*

  Data узла компонента LibraryTree предоставляет объект-заменитель,
  реализованный в виде абстрактного класса, для доступа к реальному объекту.
  Интерфейс абстрактного класса:
   - Caption - визуальный заголовок узла (Get, Set)
   - ReadOnly - флаг редактирования заголовка через процедуру SetCaption
   - Image, Selected - иконки узла (обычная и выделенного узла)
   - OwnerNode - указатель на узел, содержащий текущий объект
   - Tree - указатель на компонент
   - FormClass - классовая ссылка на форму объекта
   - Add, Edit, Delete, Clone - процедуры, изменяющие текущий объект библиотеки
   - SetProcs - набор доступных процедур данного объекта

  Конкретные классы унаследованы от абстрактного класса и  представляют собой
  обертки объектов библиотеки:
    - sublibrary (разделы библиотеки для разработчика и пользователя)
    - module (описание конкретного модуля, которое содержится в файле
      module.jlf)
    - basedescription (базовое описание модуля)
    и т.д.

*)


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Forms, Dialogs, Windows,
  uLibrary;

type

  { TEditingForm }
  TEditingFormClass = class of TEditingForm;
  TLibraryProxyData = class ;

  TEditingForm = class (TForm)
  public
    procedure Load(const aLibraryProxyData: TLibraryProxyData); virtual; abstract;
    procedure Unload; virtual; abstract;
  end;

  // Базовый класс данных для узлов LibraryTree

  { TLibraryProxyData }

  TLibraryProxyData = class
    type
    // Типы иконок узла
    TTypeImage = (
      iiNone = -1,
      iiDeveloperSublibrary,
      iiUserSublibrary,
      iiModule,
      iiDescription,
      iiPresets,
      iiPickList,
      iiRegisters,
      iiInput,
      iiHolding,
      iiVarDefine,
      iiInActiveGroups,
      iiActiveGroups
    );

    // Типы доступных процедур для изменения объекта
    TTypeProc = (
      tpAdd,
      tpEdit,
      tpDelete,
      tpClone
    );
    TSetProcs = set of TTypeProc;


  protected
    // Флаг редактирования заголовка
    fReadOnly:  Boolean;
    // Классовая ссылка на форму детализации
    fFormClass: TEditingFormClass;
    // Указатель на узел, содержащий данный объект
    fOwnerNode: PVirtualNode;
    // Указатель на компонент
    fTree:      TBaseVirtualTree;
    fSetProcs:  TSetProcs;
  protected
    function GetCaption: String; virtual; abstract;
    procedure SetCaption(const {%H-}aCaption: String); virtual;
    function GetImage: TTypeImage; virtual; abstract;
    function GetSelected: TTypeImage; virtual; abstract;

  public
    constructor Create;

  public
    procedure Add(Sender: TObject); virtual;
    procedure Edit(Sender: TObject); virtual;
    procedure Delete(Sender: TObject); virtual;
    procedure Clone(Sender: TObject); virtual;


    // Заголовок узла
    property Caption: String read GetCaption write SetCaption;
    property ReadOnly: Boolean read fReadOnly;
    // Иконка невыделенного узла
    property Image: TTypeImage read GetImage;
    // Иконка выделенного узла
    property Selected: TTypeImage read GetSelected;
    // Классовая ссылка на форму детализации
    property FormClass: TEditingFormClass read fFormClass;

    // Указатель на узел, содержащий данный объект
    property OwnerNode: PVirtualNode read fOwnerNode write fOwnerNode;
    // Указатель на компонент
    property Tree: TBaseVirtualTree read fTree write fTree;
    // Указатель набора процедур для PopupMenu
    property SetProcs: TSetProcs read fSetProcs;

  end;

  { TDeveloperSublibraryData }

  TDeveloperSublibraryData = class (TLibraryProxyData)
  protected
    fTypeImage:  TTypeImage;
    fSublibrary: ISublibrary;
    fCaption:    String;
  public
    constructor Create(const aSubLibrary: ISublibrary); reintroduce; virtual;
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  public
    procedure Add(Sender: TObject); override;
    property SubLibrary: ISubLibrary read fSubLibrary;
  end;

  { TUserSublibraryData }

  TUserSublibraryData = class (TDeveloperSublibraryData)
  public
    constructor Create(const aSubLibrary: ISublibrary); override;
  end;

  { TModuleData }

  TModuleData = class (TLibraryProxyData)
  private
    fModule: IModule;
  public
    constructor Create(const aModule: IModule); reintroduce;
  protected
    function GetCaption: String; override;
    procedure SetCaption(const aCaption: String); override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  public
    procedure Edit(Sender: TObject); override;
    procedure Delete(Sender: TObject); override;
    property Module: IModule read fModule;
  end;

  { TBaseDescriptionData }

  TBaseDescriptionData = class (TLibraryProxyData)
  private
    fBaseDescription: IBaseDescription;
  public
    constructor Create(const aBaseDescription: IBaseDescription); reintroduce;
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;

  public
    property BaseDescription: IBaseDescription read fBaseDescription;
  end;

  { TPresetsData }

  TPresetsData = class (TLibraryProxyData)
  private
    fPresets: IPresets;
  public
    constructor Create(const aPresets: IPresets); reintroduce;
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  end;

  { TBitsSetData }

  TBitsSetData = class (TLibraryProxyData)
  private
    fBitsSet: IBitsSet;
  public
    constructor Create(const aBitsSet: IBitsSet); reintroduce;
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  public
    procedure Add(Sender: TObject); override;
    property BitsSet: IBitsSet read fBitsSet;
  end;


  { TBitsData }

  TBitsData = class (TLibraryProxyData)
  private
    fBits: IBits;
  public
    constructor Create(const aBits: IBits); reintroduce;

  protected
    function GetCaption: String; override;
    procedure SetCaption(const aCaption: String); override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;

  public
    procedure Edit(Sender: TObject); override;
    procedure Delete(Sender: TObject); override;

  public
    property Bits: IBits read fBits;
  end;


  { TPickListsData }

  TPickListsData = class (TLibraryProxyData)
  private
    fPickLists: IPickLists;
  public
    constructor Create(const aPickLists: IPickLists); reintroduce;
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  public
    procedure Add(Sender: TObject); override;

  public
    property PickLists: IPickLists read fPickLists;

  end;


  { TPickListData }

  TPickListData = class (TLibraryProxyData)
  private
    fPickList: IPickList;
  public
    constructor Create(const aPickList: IPickList); reintroduce;

  protected
    function GetCaption: String; override;
    procedure SetCaption(const aCaption: String); override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;

  public
    procedure Edit(Sender: TObject); override;
    procedure Delete(Sender: TObject); override;

  public
    property PickList: IPickList read fPickList;
  end;


  { TRegistersData }

  TRegistersData = class (TLibraryProxyData)
  private
    fRegisters: IRegisters;
  public
    constructor Create(const aRegisters: IRegisters); reintroduce;
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  end;

  { TInputVarsData }

  TInputVarsData = class (TLibraryProxyData)
  private
    fVars: IVars;
  public
    constructor Create(const aVars: IVars); reintroduce;
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  public
    procedure Add(Sender: TObject); override;
  public
    property Vars: IVars read fVars;
  end;

  { THoldingVarsData }

  THoldingVarsData = class (TInputVarsData)
  protected
    function GetCaption: String; override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  end;

  { TVarDefineData }

  TVarDefineData = class (TLibraryProxyData)
  private
    fVarDefine: IVarDefine;
  public
    constructor Create(const aVarDefine: IVarDefine); reintroduce;
  protected
    function GetCaption: String; override;
    procedure SetCaption(const aCaption: String); override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  public
    procedure Edit(Sender: TObject); override;
    procedure Delete(Sender: TObject); override;
    procedure Clone(Sender: TObject); override;
    property VarDefine: IVarDefine read fVarDefine;
  end;


  { TGroupsData }

  TGroupsData = class (TLibraryProxyData)
  private
    fGroups: IGroups;
  public
    constructor Create(const aGroups: IGroups); reintroduce;
  protected
    function GetCaption: String; override;
    procedure SetCaption(const aCaption: String); override;
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  public
    procedure Add(Sender: TObject); override;
    procedure Edit(Sender: TObject); override;
    procedure Delete(Sender: TObject); override;
  end;

implementation

uses uLibraryTreeBuilder, uModuleForm, uEmptyForm, uBaseDescriptionForm,
  uPickListForm, uvarsform, uGroupForm, uBitsForm;


{ TLibraryProxyData }

constructor TLibraryProxyData.Create;
begin
  inherited Create;
  fFormClass := TEmptyForm;
  fReadOnly  := True;
  fSetProcs  := [];
end;

procedure TLibraryProxyData.SetCaption(const aCaption: String);
begin
  { TODO : Stub }
end;

procedure TLibraryProxyData.Add(Sender: TObject);
begin
  { TODO : Stub }
end;

procedure TLibraryProxyData.Edit(Sender: TObject);
begin
  { TODO : Stub }
end;

procedure TLibraryProxyData.Delete(Sender: TObject);
begin
  { TODO : Stub }
end;

procedure TLibraryProxyData.Clone(Sender: TObject);
begin
  { TODO : Stub }
end;


{ TDeveloperSublibraryData }

constructor TDeveloperSublibraryData.Create(const aSubLibrary: ISublibrary);
begin
  inherited Create;
  fCaption    := 'Модули разработчика';
  fSublibrary := aSublibrary;
  fTypeImage  := TTypeImage.iiDeveloperSublibrary;
  fSetProcs   := [tpAdd];
end;

function TDeveloperSublibraryData.GetCaption: String;
begin
  Result := fCaption;
end;

function TDeveloperSublibraryData.GetImage: TTypeImage;
begin
  Result := fTypeImage;
end;

function TDeveloperSublibraryData.GetSelected: TTypeImage;
begin
  Result := fTypeImage;
end;

procedure TDeveloperSublibraryData.Add(Sender: TObject);
var
  FoundIndex: Integer;
  UId:    Word;
  StrUid: String;
  Module: IModule;
begin
  StrUid := '';
  if not InputQuery('Новый модуль', 'Введите UID нового модуля', StrUid) then
    Exit;
  try
    UId := StrToIntDef(StrUid, 0);
    FoundIndex := fSublibrary.IndexOf(Uid);
    if FoundIndex >= 0 then
      raise Exception.Create('Модуль с данным Uid уже существует');
    FoundIndex  := fSublibrary.Add(Uid);
    Module      := fSublibrary.Data[FoundIndex];
    Module.Name := 'Новый модуль';
    TLibraryTreeDirector.Build(TModuleBuilder.Create(fTree, fOwnerNode, Module) as ILibraryTreeBuilder);

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания модуля')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;

{ TUserSublibraryData }

constructor TUserSublibraryData.Create(const aSubLibrary: ISublibrary);
begin
  inherited Create(aSublibrary);
  fCaption   := 'Модули пользователя';
  fTypeImage := TTypeImage.iiUserSublibrary;
end;

{ TModuleData }

constructor TModuleData.Create(const aModule: IModule);
begin
  inherited Create;
  fModule    := aModule;
  fReadOnly  := False;
  fFormClass := TModuleForm;
  fSetProcs  := [tpEdit, tpDelete];
end;

function TModuleData.GetCaption: String;
begin
  Result := fModule.Name;
end;

procedure TModuleData.SetCaption(const aCaption: String);
begin
  fModule.Name := aCaption;
  fTree.InvalidateNode(fOwnerNode);
end;

function TModuleData.GetImage: TTypeImage;
begin
  Result := iiModule;
end;

function TModuleData.GetSelected: TTypeImage;
begin
  Result := iiModule;
end;

procedure TModuleData.Edit(Sender: TObject);
begin
  fTree.EditNode(fOwnerNode, -1);
end;

procedure TModuleData.Delete(Sender: TObject);
var
  Parent: PVirtualNode;
  P:      Pointer;
begin
  if MessageBox(Application.MainFormHandle,
    PChar(Utf8ToAnsi(Format('Удалить модуль ''%s''?', [fModule.Name]))),
    PChar(Utf8ToAnsi('Удаление')), MB_ICONWARNING + MB_OKCANCEL) = idCancel then
    Exit;

  Parent := fOwnerNode^.Parent;
  fTree.Selected[Parent] := True;
  P      := fTree.GetNodeData(Parent);
  TDeveloperSublibraryData(P^).fSublibrary.Remove(fModule.Uid);
  fTree.DeleteNode(fOwnerNode);
end;

{ TBaseDescriptionData }

constructor TBaseDescriptionData.Create(const aBaseDescription: IBaseDescription);
begin
  inherited Create;
  fFormClass := TBaseDescriptionForm;
  fBaseDescription := aBaseDescription;
end;

function TBaseDescriptionData.GetCaption: String;
begin
  Result := 'Общая информация';
end;

function TBaseDescriptionData.GetImage: TTypeImage;
begin
  Result := iiDescription;
end;

function TBaseDescriptionData.GetSelected: TTypeImage;
begin
  Result := iiDescription;
end;

{ TPresetsData }

constructor TPresetsData.Create(const aPresets: IPresets);
begin
  inherited Create;
  fPresets := aPresets;
end;

function TPresetsData.GetCaption: String;
begin
  Result := 'Предустановленные значения';
end;

function TPresetsData.GetImage: TTypeImage;
begin
  Result := iiPresets;
end;

function TPresetsData.GetSelected: TTypeImage;
begin
  Result := iiPresets;
end;

{ TBitsSetData }

constructor TBitsSetData.Create(const aBitsSet: IBitsSet);
begin
  inherited Create;
  fBitsSet  := aBitsSet;
  fSetProcs := [tpAdd];
  fFormClass := TBitsForm;
end;

function TBitsSetData.GetCaption: String;
begin
  Result := Format('Наборы битов [%d]', [fBitsSet.Count]);
end;

function TBitsSetData.GetImage: TTypeImage;
begin
  Result := iiPickList;
end;

function TBitsSetData.GetSelected: TTypeImage;
begin
  Result := iiPickList;
end;

procedure TBitsSetData.Add(Sender: TObject);
var
  FoundIndex: Integer;
  BitsName: String;
  Bits: IBits;
begin
  BitsName := '';
  if not InputQuery('Новый набор битов', 'Введите название нового набора', BitsName) then
    Exit;
  try
    FoundIndex := fBitsSet.IndexOf(BitsName);
    if FoundIndex >= 0 then
      raise Exception.Create('Список с данным именем уже существует');
    FoundIndex := fBitsSet.Add(BitsName);
    Bits      := fBitsSet.Data[FoundIndex];
    Bits.Name := BitsName;
    TLibraryTreeDirector.Build(TBitsBuilder.Create(fTree, fOwnerNode, Bits) as ILibraryTreeBuilder);
    fTree.Expanded[fOwnerNode] := True;
  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания списка')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;


{ TBitsData }

constructor TBitsData.Create(const aBits: IBits);
begin
  inherited Create;
  fFormClass := TBitsForm;
  fBits     := aBits;
  fReadOnly := False;
  fSetProcs := [tpEdit, tpDelete];
end;

function TBitsData.GetCaption: String;
begin
  Result := fBits.Name;
end;

procedure TBitsData.SetCaption(const aCaption: String);
begin
  try
    try
      if fBits.GetLastError <> 0 then
        raise Exception.Create(Format('Набор с именем ''%s'' уже существует',
          [aCaption]));
      fBits.Name := aCaption;
    except
      on E: Exception do
        MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
          PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
      else
        MessageBox(Application.MainFormHandle,
          PChar(Utf8ToAnsi('Ошибка редактирования списка')),
          PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    end;
  finally
    fTree.InvalidateNode(fOwnerNode);
  end;
end;

function TBitsData.GetImage: TTypeImage;
begin
  Result := iiPickList;
end;

function TBitsData.GetSelected: TTypeImage;
begin
  Result := iiPickList;
end;

procedure TBitsData.Edit(Sender: TObject);
begin
  fTree.EditNode(fOwnerNode, -1);
end;

procedure TBitsData.Delete(Sender: TObject);
var
  Parent: PVirtualNode;
  P:      Pointer;
begin
  if MessageBox(Application.MainFormHandle,
    PChar(Utf8ToAnsi(Format('Удалить набор ''%s''?', [fBits.Name]))),
    PChar(Utf8ToAnsi('Удаление')), MB_ICONWARNING + MB_OKCANCEL) = idCancel then
    Exit;

  Parent := fOwnerNode^.Parent;
  fTree.Selected[Parent] := True;
  P      := fTree.GetNodeData(Parent);
  TBitsSetData(P^).fBitsSet.Remove(fBits.Name);
  fTree.DeleteNode(fOwnerNode);
end;

{ TPickListsData }

constructor TPickListsData.Create(const aPickLists: IPickLists);
begin
  inherited Create;
  fPickLists := aPickLists;
  fSetProcs  := [tpAdd];
   fFormClass := TPickListForm;
end;

function TPickListsData.GetCaption: String;
begin
  Result := Format('Списки [%d]', [fPickLists.Count]);
end;

function TPickListsData.GetImage: TTypeImage;
begin
  Result := iiPickList;
end;

function TPickListsData.GetSelected: TTypeImage;
begin
  Result := iiPickList;
end;

procedure TPickListsData.Add(Sender: TObject);
var
  FoundIndex: Integer;
  ListName:   String;
  PickList:   IPickList;
begin
  ListName := '';
  if not InputQuery('Новый список', 'Введите название нового списка', ListName) then
    Exit;
  try
    FoundIndex := fPickLists.IndexOf(ListName);
    if FoundIndex >= 0 then
      raise Exception.Create('Список с данным именем уже существует');
    FoundIndex    := fPickLists.Add(ListName);
    PickList      := fPickLists.Data[FoundIndex];
    PickList.Name := ListName;
    TLibraryTreeDirector.Build(TPickListBuilder.Create(fTree, fOwnerNode, PickList) as ILibraryTreeBuilder);
    fTree.Expanded[fOwnerNode] := True;

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания списка')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;

{ TPickListData }

constructor TPickListData.Create(const aPickList: IPickList);
begin
  inherited Create;
  fFormClass := TPickListForm;
  fPickList  := aPickList;
  fReadOnly  := False;
  fSetProcs  := [tpEdit, tpDelete];
end;

function TPickListData.GetCaption: String;
begin
  Result := fPickList.Name;
end;

procedure TPickListData.SetCaption(const aCaption: String);
begin
  try
    try
      if fPickList.GetLastError <> 0 then
        raise Exception.Create(Format('Список с именем ''%s'' уже существует',
          [aCaption]));
      fPickList.Name := aCaption;
    except
      on E: Exception do
        MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
          PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
      else
        MessageBox(Application.MainFormHandle,
          PChar(Utf8ToAnsi('Ошибка редактирования списка')),
          PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    end;
  finally
    fTree.InvalidateNode(fOwnerNode);
  end;
end;

function TPickListData.GetImage: TTypeImage;
begin
  Result := iiPickList;
end;

function TPickListData.GetSelected: TTypeImage;
begin
  Result := iiPickList;
end;

procedure TPickListData.Edit(Sender: TObject);
begin
  fTree.EditNode(fOwnerNode, -1);
end;

procedure TPickListData.Delete(Sender: TObject);
var
  Parent: PVirtualNode;
  P:      Pointer;
begin
  if MessageBox(Application.MainFormHandle,
    PChar(Utf8ToAnsi(Format('Удалить список ''%s''?', [fPickList.Name]))),
    PChar(Utf8ToAnsi('Удаление')), MB_ICONWARNING + MB_OKCANCEL) = idCancel then
    Exit;

  Parent := fOwnerNode^.Parent;
  fTree.Selected[Parent] := True;
  P      := fTree.GetNodeData(Parent);
  TPickListsData(P^).fPickLists.Remove(fPickList.Name);
  fTree.DeleteNode(fOwnerNode);
end;

{ TRegistersData }

constructor TRegistersData.Create(const aRegisters: IRegisters);
begin
  inherited Create;
  fRegisters := aRegisters;
end;

function TRegistersData.GetCaption: String;
begin
  Result := 'Регистры';
end;

function TRegistersData.GetImage: TTypeImage;
begin
  Result := iiRegisters;
end;

function TRegistersData.GetSelected: TTypeImage;
begin
  Result := iiRegisters;
end;

{ TInputVarsData }

constructor TInputVarsData.Create(const aVars: IVars);
begin
  inherited Create;
  fVars     := aVars;
  fSetProcs := [tpAdd];
  fFormClass := TVarsForm;
end;

function TInputVarsData.GetCaption: String;
begin
  Result := Format('Input [%d]', [fVars.Count]);
end;

function TInputVarsData.GetImage: TTypeImage;
begin
  Result := iiInput;
end;

function TInputVarsData.GetSelected: TTypeImage;
begin
  Result := iiInput;
end;

procedure TInputVarsData.Add(Sender: TObject);
var
  IndexStr:   String;
  Index:      Integer;
  VarDefine:  IVarDefine;
  FoundIndex: Integer;
begin
  IndexStr := '';
  if not InputQuery('Новый регистр', 'Введите индекс нового регистра', IndexStr) then
    Exit;
  Index := StrToIntDef(IndexStr, 0);

  FoundIndex     := fVars.Add(GetNewGuid);
  VarDefine      := fVars.Data[FoundIndex];
  VarDefine.Index := Index;
  VarDefine.Name := 'Новый регистр';

  TLibraryTreeDirector.Build(TInsertedVarDefineBuilder.Create(fTree,
    fOwnerNode, VarDefine) as ILibraryTreeBuilder);
end;

{ THoldingVarsData }

function THoldingVarsData.GetCaption: String;
begin
  Result := Format('Holding [%d]', [fVars.Count]);
end;

function THoldingVarsData.GetImage: TTypeImage;
begin
  Result := iiHolding;
end;

function THoldingVarsData.GetSelected: TTypeImage;
begin
  Result := iiHolding;
end;

{ TVarDefineData }

constructor TVarDefineData.Create(const aVarDefine: IVarDefine);
begin
  inherited Create;
  fVarDefine := aVarDefine;
  fSetProcs  := [tpEdit, tpDelete, tpClone];
  fFormClass := TEmptyForm;
  fReadOnly  := False;
end;

function TVarDefineData.GetCaption: String;
begin
  Result := Format('[%d] %s', [fVarDefine.Index, fVarDefine.Name]);
end;

procedure TVarDefineData.SetCaption(const aCaption: String);
begin
  fVarDefine.Name := aCaption;
  fTree.InvalidateNode(fOwnerNode);
end;

function TVarDefineData.GetImage: TTypeImage;
begin
  Result := iiVarDefine;
end;

function TVarDefineData.GetSelected: TTypeImage;
begin
  Result := iiVarDefine;
end;

procedure TVarDefineData.Edit(Sender: TObject);
begin
  fTree.EditNode(fOwnerNode, -1);
end;

procedure TVarDefineData.Delete(Sender: TObject);
var
  Parent: PVirtualNode;
  P:      Pointer;
begin
  if MessageBox(Application.MainFormHandle,
    PChar(Utf8ToAnsi(Format('Удалить регистр ''%s''?', [GetCaption]))),
    PChar(Utf8ToAnsi('Удаление')), MB_ICONWARNING + MB_OKCANCEL) = idCancel then
    Exit;

  Parent := fOwnerNode^.Parent;
  fTree.Selected[Parent] := True;
  P      := fTree.GetNodeData(Parent);
  TInputVarsData(P^).fVars.Remove(fVarDefine.Uid);
  fTree.DeleteNode(fOwnerNode);
end;

procedure TVarDefineData.Clone(Sender: TObject);

  procedure DeepClone(const aNewVarDefine: IVarDefine);

    procedure BitClone(const aNewBits: IBits);
    var
      P:   Pointer;
      Bit: Byte;
      BitDefine: IBitDefine;
    begin
      for P in fVarDefine.Bits do
      begin
        Bit := fVarDefine.Bits.ExtractKey(P);
        BitDefine := fVarDefine.Bits.ExtractData(P);
        aNewBits.Add(Bit);
        aNewBits[Bit].Index := BitDefine.Index;
        aNewBits[Bit].Ver   := BitDefine.Ver;
        aNewBits[Bit].Name  := BitDefine.Name;
        aNewBits[Bit].ShortDescription := BitDefine.ShortDescription;
        aNewBits[Bit].Description.Text := BitDefine.Description.Text;
      end;
    end;

    procedure PickClone(const aNewPickList: IPickList);
    var
      P: Pointer;
      PickValue: Word;
      PickItem: IPickItem;
    begin
      for P in fVarDefine.Picklist do
      begin
        PickValue := fVarDefine.PickList.ExtractKey(P);
        PickItem  := fVarDefine.PickList.ExtractData(P);
        aNewPickList.Add(PickValue);
        aNewPickList[PickValue].Value := PickItem.Value;
        aNewPickList[PickValue].Ver   := PickItem.Ver;
        aNewPickList[PickValue].Name  := PickItem.Name;
        aNewPickList[PickValue].ShortDescription := PickItem.ShortDescription;
        aNewPickList[PickValue].Description.Text := PickItem.Description.Text;
      end;

    end;

  begin
    aNewVarDefine.Access := fVarDefine.Access;
    aNewVarDefine.Kind := fVarDefine.Kind;
    aNewVarDefine.Measure := fVarDefine.Measure;
    aNewVarDefine.Multipler := fVarDefine.Multipler;
    aNewVarDefine.ReadAlways := fVarDefine.ReadAlways;
    aNewVarDefine.ShortDescription := fVarDefine.ShortDescription;
    aNewVarDefine.SingleRequest := fVarDefine.SingleRequest;
    aNewVarDefine.Synchronization := fVarDefine.Synchronization;
    aNewVarDefine.VarType := fVarDefine.VarType;
    aNewVarDefine.Ver := fVarDefine.Ver;
    aNewVarDefine.Description.Text := fVarDefine.Description.Text;

    BitClone(aNewVarDefine.Bits);
    PickClone(aNewVarDefine.Picklist);

  end;

var
  IndexStr:   String;
  Index:      Integer;
  ClonedVarDefine: IVarDefine;
  FoundIndex: Integer;

  P: Pointer;

begin
  IndexStr := '';
  if not InputQuery('Новый регистр', 'Введите индекс нового регистра', IndexStr) then
    Exit;
  Index := StrToIntDef(IndexStr, 0);

  P := fTree.GetNodeData(fOwnerNode^.Parent);
  if not (TLibraryProxyData(P^) is TInputVarsData) then
    Exit;

  FoundIndex      := TInputVarsData(P^).fVars.Add(GetNewGuid);
  ClonedVarDefine := TInputVarsData(P^).fVars.Data[FoundIndex];


  ClonedVarDefine.Index := Index;
  ClonedVarDefine.Name  := Format('%s_c', [fVarDefine.Name]);
  DeepClone(ClonedVarDefine);

  TLibraryTreeDirector.Build(TInsertedVarDefineBuilder.Create(fTree,
    fOwnerNode^.Parent, ClonedVarDefine) as ILibraryTreeBuilder);

end;


{ TGroupsData }

constructor TGroupsData.Create(const aGroups: IGroups);
begin
  inherited Create;

  fGroups := aGroups;
  if fGroups.ShortDescription = '' then
    fSetProcs := [tpAdd]
  else
  begin
    fSetProcs  := [tpAdd, tpEdit, tpDelete];
    fReadOnly  := False;
    fFormClass := TGroupForm;
  end;
end;

function TGroupsData.GetCaption: String;
begin
  if fGroups.ShortDescription = '' then
    Result := format('Конфигурация [%d %d]', [fGroups.GroupsList.Count,
      fGroups.GetGroup.Count])
  else
    Result := format('%s [%d %d]', [fGroups.ShortDescription,
      fGroups.GroupsList.Count, fGroups.GetGroup.Count]);
end;

procedure TGroupsData.SetCaption(const aCaption: String);
begin
  if fGroups.ShortDescription = '' then
    Exit;
  fGroups.ShortDescription := aCaption;
  fTree.InvalidateNode(fOwnerNode);
end;

function TGroupsData.GetImage: TTypeImage;
begin
  Result := iiInActiveGroups;
end;

function TGroupsData.GetSelected: TTypeImage;
begin
  Result := iiActiveGroups;
end;

procedure TGroupsData.Add(Sender: TObject);
var
  Index:  Integer;
  Name:   String;
  Groups: IGroups;
begin
  Name := '';
  if not InputQuery('Новая группа конфигураций', 'Введите название группы', Name) then
    Exit;
  try
    Index  := fGroups.GroupsList.AddGroups(Name);
    Groups := fGroups.GroupsList[Index];

    TLibraryTreeDirector.Build(TGroupsBuilder.Create(fTree, fOwnerNode, Groups) as ILibraryTreeBuilder);

    if fOwnerNode^.LastChild <> nil then
    begin
      fTree.Selected[fOwnerNode^.LastChild] := True;
      fTree.ScrollIntoView(fOwnerNode^.LastChild, True);
    end;

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания группы')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure TGroupsData.Edit(Sender: TObject);
begin
  fTree.EditNode(fOwnerNode, -1);
end;

procedure TGroupsData.Delete(Sender: TObject);
var
  Parent: PVirtualNode;
  P:      Pointer;
begin
  if MessageBox(Application.MainFormHandle,
    PChar(Utf8ToAnsi(Format('Удалить группу конфигурации ''%s''?',
    [fGroups.ShortDescription]))), PChar(Utf8ToAnsi('Удаление')),
    MB_ICONWARNING + MB_OKCANCEL) = idCancel then
    Exit;

  Parent := fOwnerNode^.Parent;
  fTree.Selected[Parent] := True;
  P      := fTree.GetNodeData(Parent);
  TGroupsData(P^).fGroups.GetGroupsList.Remove(fGroups);
  fTree.DeleteNode(fOwnerNode);

end;

end.
