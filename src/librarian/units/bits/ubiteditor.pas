unit uBitEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Windows, VirtualTrees,

  uLibrary,
  uVtBaseEditor,
  uPropertyEditorNew,
  uAdvancedPropertyEditor;

type

  { TBitData }

  TBitData = class(TPropertyData)
  protected
    fBit: IBitDefine;
  public
    property Bit: IBitDefine read fBit;
  end;


  { TIndexBitData }

  TIndexBitData = class(TBitData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aBit: IBitDefine); reintroduce;
  end;


  { TNameBitData }

  TNameBitData = class(TBitData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aBit: IBitDefine); reintroduce;
  end;


  { TShortDescriptionBitData }

  TShortDescriptionBitData = class(TBitData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aBit: IBitDefine); reintroduce;
  end;


  { TDescriptionBitData }

  TDescriptionBitData = class(TBitData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aBit: IBitDefine); reintroduce;
  end;

  { TVerBitData }

  TVerBitData = class(TBitData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aBit: IBitDefine); reintroduce;
  end;


  { TBitEditor }

  TBitEditor = class(TAdvancedPropertyEditor)
  private
    fBits: IBits;

  private
    procedure SetBits(const aBits: IBits);
    procedure AddBit(const aBit: IBitDefine);
    procedure ConstructNewBit(const aClonedBit: IBitDefine = nil);

  protected
    procedure AddNewItem(Sender: TObject); override;
    procedure CloneItem(Sender: TObject);  override;
    procedure DeleteItem(Sender: TObject); override;

  public
    property Bits: IBits read fBits write SetBits;
  end;


implementation

{ TVerBitData }

constructor TVerBitData.Create(const aBit: IBitDefine);
begin
  inherited Create;
  Key := 'Версия';
  fBit := aBit;
end;

function TVerBitData.GetValue: string;
begin
  Result := fBit.Ver;
end;

procedure TVerBitData.SetValue(const aValue: string);
begin
  fBit.Ver := aValue;
end;

{ TDescriptionBitData }

constructor TDescriptionBitData.Create(const aBit: IBitDefine);
begin
  inherited Create;
  Key := 'Описание';
  fBit := aBit;
end;

function TDescriptionBitData.GetValue: string;
begin
  Result := fBit.Description.Text;
end;

procedure TDescriptionBitData.SetValue(const aValue: string);
begin
  fBit.Description.Text := aValue;
end;

{ TShortDescriptionBitData }
constructor TShortDescriptionBitData.Create(const aBit: IBitDefine);
begin
  inherited Create;
  Key := 'Краткое описание';
  fBit := aBit;
end;

function TShortDescriptionBitData.GetValue: string;
begin
  Result := fBit.ShortDescription;
end;

procedure TShortDescriptionBitData.SetValue(const aValue: string);
begin
  fBit.ShortDescription := aValue;
end;

{ TNameBitData }
constructor TNameBitData.Create(const aBit: IBitDefine);
begin
  inherited Create;
  Key := 'Имя';
  fBit := aBit;
end;

function TNameBitData.GetValue: string;
begin
  Result := fBit.Name;
end;

procedure TNameBitData.SetValue(const aValue: string);
begin
  fBit.Name := aValue;
end;

{ TIndexBitData }

constructor TIndexBitData.Create(const aBit: IBitDefine);
begin
  inherited Create;
  Key := 'Индекс';
  fBit := aBit;
end;

function TIndexBitData.GetValue: string;
begin
  Result := IntToStr(fBit.Index);
end;

procedure TIndexBitData.SetValue(const aValue: string);
begin
  try
    fBit.Index := StrToIntDef(aValue, 0);
    if fBit.GetLastError <> 0 then
      raise Exception.Create('Дубликат индекса');
  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;

function TIndexBitData.GetEditLink: IVtEditLink;
begin
  Result := TVtIntegerEditLink.Create(0, 32);
end;

{ TBitEditor }

procedure TBitEditor.SetBits(const aBits: IBits);
var
  P: Pointer;
  Bit: IBitDefine;
begin
  if fBits <> aBits then
    fBits := aBits;

  Clear;
  for P in fBits do
  begin
    Bit := fBits.ExtractData(P);
    AddBit(Bit);
  end;

end;

procedure TBitEditor.AddBit(const aBit: IBitDefine);
var
  Node: PVirtualNode;
begin
  Node := AddChild(nil, TIndexBitData.Create(aBit));
  AddChild(Node, TNameBitData.Create(aBit));
  AddChild(Node, TVerBitData.Create(aBit));
  AddChild(Node, TShortDescriptionBitData.Create(aBit));
  Node := AddChild(Node, TDescriptionBitData.Create(aBit));
  Node^.NodeHeight := 3 * Node^.NodeHeight;
  Node^.States := Node^.States + [vsMultiline];
end;

procedure TBitEditor.ConstructNewBit(const aClonedBit: IBitDefine);
var
  IndexStr: string;
  Index: word;
  Bit: IBitDefine;
begin

  IndexStr := '';
  if not InputQuery('Новый индекс', 'Введите новый индекс бита',
    IndexStr) then
    Exit;
  Index := StrToIntDef(IndexStr, 0);

  try
    if not ((Index >= 0)  and (Index < 32)) then
	    raise Exception.Create(Format('Индекс ''%d'' вне диапазона допустимых значений: 0-31', [Index]));

    fBits.Add(Index);

    if fBits.GetLastError <> 0 then
      raise Exception.Create('Бит с данным индексом уже существует');

    Bit := fBits[Index];

    if aClonedBit <> nil then
      Bit.Copy(aClonedBit);


    AddBit(Bit);

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания бита')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure TBitEditor.AddNewItem(Sender: TObject);
begin
  ConstructNewBit;
end;

procedure TBitEditor.CloneItem(Sender: TObject);
begin
  ConstructNewBit(TBitData(GetData(FocusedNode)).Bit);
end;

procedure TBitEditor.DeleteItem(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := FocusedNode;

  while GetNodeLevel(Node) > 1 do
    Node := Node^.Parent;

  if MessageBox(Application.MainFormHandle,
    PChar(Utf8ToAnsi(Format('Удалить бит ''%s'' из набора?',
    [TBitData(GetData(Node)).Bit.Name]))), PChar(Utf8ToAnsi('Удаление')),
    MB_ICONWARNING + MB_OKCANCEL) = idCancel then
    Exit;

  fBits.Remove(TBitData(GetData(Node)).Bit.Index);

  DeleteNode(Node);

end;




end.

