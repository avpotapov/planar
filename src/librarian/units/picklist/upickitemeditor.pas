unit uPickItemEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Windows, Forms, Menus, LResources, Controls,
  Dialogs,
  uLibrary,
  uPropertyEditorNew,
  uAdvancedPropertyEditor,
  uVtBaseEditor;

type

  { TPickItemData }

  TPickItemData = class(TPropertyData)
  protected
    fPickItem: IPickItem;
  public
    property PickItem: IPickItem read fPickItem;
  end;

  { TValuePickData }

  TValuePickData = class(TPickItemData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aPickItem: IPickItem); reintroduce;
  end;

  { TNamePickData }

  TNamePickData = class(TPickItemData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aPickItem: IPickItem); reintroduce;
  end;

  { TShortDescriptionPickData }

  TShortDescriptionPickData = class(TPickItemData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aPickItem: IPickItem); reintroduce;
  end;

  { TDescriptionPickData }

  TDescriptionPickData = class(TPickItemData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aPickItem: IPickItem); reintroduce;
  end;

  { TVerPickData }

  TVerPickData = class(TPickItemData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aPickItem: IPickItem); reintroduce;
  end;

  { TPickItemEditor }

  TPickItemEditor = class(TAdvancedPropertyEditor)
  private
    fPickList: IPickList;

  private
    procedure SetPickList(const aPickList: IPickList);
    procedure AddPickItem(const aPickItem: IPickItem);
    procedure ConstructNewPickItem(const aClonedPickItem: IPickItem = nil);
  protected

    procedure AddNewItem(Sender: TObject); override;
    procedure CloneItem(Sender: TObject);  override;
    procedure DeleteItem(Sender: TObject); override;

  public
    property PickList: IPickList read fPickList write SetPickList;
  end;


implementation


{ TVerPickData }

function TVerPickData.GetValue: string;
begin
  Result := fPickItem.Ver;
end;

procedure TVerPickData.SetValue(const aValue: string);
begin
  fPickItem.Ver := aValue;
end;

constructor TVerPickData.Create(const aPickItem: IPickItem);
begin
  inherited Create;
  fPickItem := aPickItem;
  Key := 'Версия';
end;

{ TShortDescriptionPickData }

function TShortDescriptionPickData.GetValue: string;
begin
  Result := fPickItem.ShortDescription;
end;

procedure TShortDescriptionPickData.SetValue(const aValue: string);
begin
  fPickItem.ShortDescription := aValue;
end;

constructor TShortDescriptionPickData.Create(const aPickItem: IPickItem);
begin
  inherited Create;
  fPickItem := aPickItem;
  Key := 'Краткое описание';
end;

{ TDescriptionPickData }

function TDescriptionPickData.GetValue: string;
begin
  Result := fPickItem.Description.Text;
end;

procedure TDescriptionPickData.SetValue(const aValue: string);
begin
  fPickItem.Description.Text := aValue;
end;

function TDescriptionPickData.GetEditLink: IVtEditLink;
begin
  Result := TVtMemoEditLink.Create;
end;

constructor TDescriptionPickData.Create(const aPickItem: IPickItem);
begin
  inherited Create;
  fPickItem := aPickItem;
  Key := 'Описание';
end;

{ TNamePickData }

function TNamePickData.GetValue: string;
begin
  Result := fPickItem.Name;
end;

procedure TNamePickData.SetValue(const aValue: string);
begin
  fPickItem.Name := aValue;
end;

constructor TNamePickData.Create(const aPickItem: IPickItem);
begin
  inherited Create;
  fPickItem := aPickItem;
  Key := 'Название';
end;

{ TValuePickData }

function TValuePickData.GetValue: string;
begin
  Result := IntToStr(fPickItem.Value);
end;

procedure TValuePickData.SetValue(const aValue: string);
begin
  try
    fPickItem.Value := StrToIntDef(aValue, 0);
    if fPickItem.GetLastError <> 0 then
      raise Exception.Create('Дубликат значения списка');
  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;

function TValuePickData.GetEditLink: IVtEditLink;
begin
  Result := TVtIntegerEditLink.Create(0, 65535);
end;

constructor TValuePickData.Create(const aPickItem: IPickItem);
begin
  inherited Create;
  fPickItem := aPickItem;
  Key := 'Значение';
end;

{ TPickItemEditor }

procedure TPickItemEditor.SetPickList(const aPickList: IPickList);
var
  P: Pointer;
  PickItem: IPickItem;
begin
  if fPickList <> aPickList then
    fPickList := aPickList;

  Clear;
  for P in fPickList do
  begin
    PickItem := fPickList.ExtractData(P);
    AddPickItem(PickItem);
  end;

end;


procedure TPickItemEditor.ConstructNewPickItem(const aClonedPickItem: IPickItem);
var
  ValueStr: string;
  Value: word;
  PickItem: IPickItem;
begin

  ValueStr := '';
  if not InputQuery('Новое значение', 'Введите значение клонированного элемента списка',
    ValueStr) then
    Exit;
  Value := StrToIntDef(ValueStr, 0);

  try
    fPickList.Add(Value);

    if fPickList.GetLastError <> 0 then
      raise Exception.Create('Элемент списка с данным значением уже существует');

    PickItem := fPickList[Value];

    if aClonedPickItem <> nil then
      PickItem.Copy(aClonedPickItem);

    AddPickItem(PickItem);

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания списка')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;
procedure TPickItemEditor.AddNewItem(Sender: TObject);
begin
  ConstructNewPickItem;
end;

procedure TPickItemEditor.CloneItem(Sender: TObject);
begin
  ConstructNewPickItem(TPickItemData(GetData(FocusedNode)).PickItem);
end;

procedure TPickItemEditor.AddPickItem(const aPickItem: IPickItem);
var
  Node: PVirtualNode;
begin
  Node := AddChild(nil, TValuePickData.Create(aPickItem));
  AddChild(Node, TNamePickData.Create(aPickItem));
  AddChild(Node, TVerPickData.Create(aPickItem));
  AddChild(Node, TShortDescriptionPickData.Create(aPickItem));
  Node := AddChild(Node, TDescriptionPickData.Create(aPickItem));
  Node^.NodeHeight := 3 * Node^.NodeHeight;
  Node^.States := Node^.States + [vsMultiline];
end;

procedure TPickItemEditor.DeleteItem(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := FocusedNode;

  while GetNodeLevel(Node) > 1 do
    Node := Node^.Parent;

  if MessageBox(Application.MainFormHandle,
    PChar(Utf8ToAnsi(Format('Удалить элемент списка ''%s''?',
    [TPickItemData(GetData(Node)).PickItem.Name]))), PChar(Utf8ToAnsi('Удаление')),
    MB_ICONWARNING + MB_OKCANCEL) = idCancel then
    Exit;

  fPickList.Remove(TPickItemData(GetData(Node)).PickItem.Value);

  DeleteNode(Node);
end;

end.
