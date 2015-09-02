unit uPickListEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Menus, Dialogs, Windows, Forms,
  Controls,
  u2ColumnEditor,
  uLibrary;

type

  { TPickListData }

  TPickListData = class(T2ColumnData)
  private
    fPickList: IPickList;
  protected
    function GetShortDescription: string; override;
    function GetName: string; override;
    procedure SetName(const aName: string); override;
    procedure SetShortDescription(const aShortDescription: string); override;
  public
    constructor Create(const aPickList: IPickList); reintroduce;
  public
    property PickList: IPickList read fPickList;
  end;


  { TPickListEditor }

  TPickListSelectEvent = procedure(Sender: TObject;
    const aPickList: IPickList) of object;

  TPickListEditor = class(T2ColumnEditor)
  private
    fOnPickListSelect: TPickListSelectEvent;
    fPickLists: IPickLists;
  private
    procedure ConstructNewPickList(const aClonedPickList: IPickList = nil);
    procedure SetPickLists(const aPickLists: IPickLists);

  protected
    procedure AddNewItem(Sender: TObject); override;
    procedure CloneItem(Sender: TObject);  override;
    procedure DeleteItem(Sender: TObject); override;
    procedure DoChange(Node: PVirtualNode); override;

  public
    property OnPickListSelect: TPickListSelectEvent
      read fOnPickListSelect write fOnPickListSelect;

    property PickLists: IPickLists read fPickLists write SetPickLists;

  end;

implementation

uses
  uVtBaseEditor;

{ TPickListData }
constructor TPickListData.Create(const aPickList: IPickList);
begin
  inherited Create;
  fPickList := aPickList;
end;

function TPickListData.GetShortDescription: string;
begin
  Result := fPickList.ShortDescription;
end;

function TPickListData.GetName: string;
begin
  Result := fPickList.Name;
end;

procedure TPickListData.SetName(const aName: string);
begin
  fPickList.Name := aName;
end;

procedure TPickListData.SetShortDescription(const aShortDescription: string);
begin
  fPickList.ShortDescription := aShortDescription;
end;

{ TPickListEditor }

procedure TPickListEditor.ConstructNewPickList(const aClonedPickList: IPickList);
var
  NameStr: string;
  PickList: IPickList;
  P: Pointer;
  PickItem: IPickItem;
begin
  NameStr := '';
  if not InputQuery('Новый список', 'Введите название списка', NameStr) then
    Exit;

  try
    fPickLists.Add(NameStr);

    if fPickLists.GetLastError <> 0 then
      raise Exception.Create('Список с данным именем уже существует');

    PickList := fPickLists[NameStr];

    if aClonedPickList <> nil then
    begin
      PickList.ShortDescription := aClonedPickList.ShortDescription;
      for P in aClonedPickList do
      begin
         PickItem := aClonedPickList.ExtractData(P);
         PickList.Add(PickItem.Value);
         PickList[PickItem.Value].Copy(PickItem);
      end;
    end;

    AddChild(nil, TPickListData.Create(PickList));

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания списка')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure TPickListEditor.AddNewItem(Sender: TObject);
begin
  ConstructNewPickList;
end;

procedure TPickListEditor.CloneItem(Sender: TObject);
begin
  ConstructNewPickList(TPickListData(GetData(FocusedNode)).PickList);
end;

procedure TPickListEditor.DeleteItem(Sender: TObject);
begin
  if MessageBox(Application.MainFormHandle,
      PChar(Utf8ToAnsi(Format('Удалить список ''%s''?',
      [TPickListData(GetData(FocusedNode)).PickList.Name]))), PChar(Utf8ToAnsi('Удаление')),
      MB_ICONWARNING + MB_OKCANCEL) = idCancel then
      Exit;

  fPickLists.Remove(GetData(FocusedNode).Name);
   DeleteNode(FocusedNode);
end;

procedure TPickListEditor.SetPickLists(const aPickLists: IPickLists);
var
P: Pointer;
PickList: IPickList;
begin
  if fPickLists <> aPickLists then
  	fPickLists := aPickLists;

  Clear;
  for P in fPickLists do
  begin
    PickList := fPickLists.ExtractData(P);
    AddChild(nil, TPickListData.Create(PickList));
  end;

  Header.Treeview.SortTree(0, sdAscending, False);
  Header.SortColumn := 0;
  Header.SortDirection := sdAscending;
end;

procedure TPickListEditor.DoChange(Node: PVirtualNode);
begin
    if Node <> nil then
    if Assigned(fOnPickListSelect) then
      fOnPickListSelect(Self, TPickListData(GetData(Node)).PickList);
  inherited DoChange(Node);
end;

end.

