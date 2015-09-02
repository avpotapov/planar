unit uBitsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Menus, Dialogs, Windows, Forms,
  Controls,
  uLibrary,
  u2ColumnEditor;

type

  { TBitsData }

  TBitsData = class(T2ColumnData)
  private
    fBits: IBits;
  protected
    function GetShortDescription: string; override;
    function GetName: string; override;
    procedure SetName(const aName: string); override;
    procedure SetShortDescription(const aShortDescription: string); override;
  public
    constructor Create(const aBits: IBits); reintroduce;
  public
    property Bits: IBits read fBits;
  end;


  TBitsSelectEvent = procedure(Sender: TObject;
    const aBits: IBits) of object;

  { TBitsEditor }

  TBitsEditor = class(T2ColumnEditor)
  private
    fOnBitsSelect: TBitsSelectEvent;
    fBitsSet: IBitsSet;

  private
    procedure SetBitsSet(const aBitsSet: IBitsSet);
    procedure ConstructNewBits(const aClonedBits: IBits = nil);

  protected

    procedure AddNewItem(Sender: TObject); override;
    procedure CloneItem(Sender: TObject);  override;
    procedure DeleteItem(Sender: TObject); override;

    procedure DoChange(Node: PVirtualNode); override;

  public
    property OnBitsSelect: TBitsSelectEvent
      read fOnBitsSelect write fOnBitsSelect;

    property BitsSet: IBitsSet read fBitsSet write SetBitsSet;

  end;

implementation

{ TBitsData }

constructor TBitsData.Create(const aBits: IBits);
begin
	inherited Create;
  fBits := aBits;
end;


function TBitsData.GetShortDescription: string;
begin
	Result := fBits.ShortDescription;
end;

function TBitsData.GetName: string;
begin
	Result := fBits.Name;
end;

procedure TBitsData.SetName(const aName: string);
begin
	fBits.Name := aName;
  try

    if fBits.GetLastError <> 0 then
  		raise Exception.Create(Format('Набор битов ''%s'' уже существует', [aName]));

  except
      on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;

procedure TBitsData.SetShortDescription(const aShortDescription: string);
begin
	fBits.ShortDescription := aShortDescription;
end;



{ TBitsEditor }

procedure TBitsEditor.ConstructNewBits(const aClonedBits: IBits);
var
  NameStr: string;
  Bits: IBits;
  P: Pointer;
  Bit: IBitDefine;
begin
  NameStr := '';
  if not InputQuery('Новый набор битов', 'Введите название нового набора битов', NameStr) then
    Exit;

  try
    fBitsSet.Add(NameStr);

    if fBitsSet.GetLastError <> 0 then
      raise Exception.Create('Набор битов с данным именем уже существует');

    Bits := fBitsSet[NameStr];

    if aClonedBits <> nil then
    begin
      Bits.ShortDescription := aClonedBits.ShortDescription;
      for P in aClonedBits do
      begin
         Bit := aClonedBits.ExtractData(P);
         Bits.Add(Bit.Index);
         Bits[Bit.Index].Copy(Bit);
      end;
    end;

    AddChild(nil, TBitsData.Create(Bits));

  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
    else
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка создания набора битов')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure TBitsEditor.AddNewItem(Sender: TObject);
begin
  ConstructNewBits;
end;

procedure TBitsEditor.CloneItem(Sender: TObject);
begin
  ConstructNewBits(TBitsData(GetData(FocusedNode)).Bits);
end;

procedure TBitsEditor.DeleteItem(Sender: TObject);
begin
  if MessageBox(Application.MainFormHandle,
      PChar(Utf8ToAnsi(Format('Удалить набор битов ''%s''?',
      [TBitsData(GetData(FocusedNode)).Bits.Name]))), PChar(Utf8ToAnsi('Удаление')),
      MB_ICONWARNING + MB_OKCANCEL) = idCancel then
      Exit;

  fBitsSet.Remove(GetData(FocusedNode).Name);
  DeleteNode(FocusedNode);
end;

procedure TBitsEditor.SetBitsSet(const aBitsSet: IBitsSet);
var
	P: Pointer;
	Bits: IBits;
begin
  if fBitsSet <> aBitsSet then
  	fBitsSet := aBitsSet;

  Clear;
  for P in fBitsSet do
  begin
    Bits := fBitsSet.ExtractData(P);
    AddChild(nil, TBitsData.Create(Bits));
  end;

  Header.Treeview.SortTree(0, sdAscending, False);
  Header.SortColumn := 0;
  Header.SortDirection := sdAscending;

end;

procedure TBitsEditor.DoChange(Node: PVirtualNode);
begin
    if Node <> nil then
    if Assigned(fOnBitsSelect) then
      fOnBitsSelect(Self, TBitsData(GetData(Node)).Bits);
  inherited DoChange(Node);
end;


end.

