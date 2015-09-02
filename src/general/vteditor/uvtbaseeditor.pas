unit uVtBaseEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Windows, StdCtrls, EditBtn, Spin, ComboEx,
  VirtualTrees, LResources;

type

  { TVtBaseData }

  TVtBaseData = class
  protected
    fReadOnly: boolean;
  protected
    function GetImageIndex: integer; virtual;
    function GetSelectedIndex: integer; virtual;
    function GetEditLink: IVtEditLink; virtual;
  public
    property ReadOnly: boolean read fReadOnly default False;
    property ImageIndex: integer read GetImageIndex default -1;
    property SelectedIndex: integer read GetSelectedIndex default -1;
    property EditLink: IVtEditLink read GetEditLink;
  end;

  { TVtBaseEditLink }

  TVtBaseEditLink = class(TInterfacedObject, IVtEditLink)
  private
    fEdit: TWinControl;
    fTree: TVirtualStringTree;
    fNode: PVirtualNode;
    fColumn: integer;

  private
    function GetBounds: TRect; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;

  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; virtual; stdcall;
    function BeginEdit: boolean; virtual; stdcall;
    function CancelEdit: boolean; virtual; stdcall;
    function EndEdit: boolean; virtual; stdcall;

  public
    destructor Destroy; override;
  end;

  { TVtComboEditLink }

  TVtComboEditLink = class(TVtBaseEditLink)
  public
    constructor Create(const aCommaText: string); reintroduce; overload;
    constructor Create(const aStrings: TStrings); reintroduce; overload;

  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; override; stdcall;
    function BeginEdit: boolean; override; stdcall;
    function EndEdit: boolean; override; stdcall;

    procedure KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CloseUp(Sender: TObject);
  end;

  { TVtIntegerEditLink }

  TVtIntegerEditLink = class(TVtBaseEditLink)
  private
    fMin: Integer;
    fMax: Integer;
    fInc: Integer;
  public
    constructor Create(const aMin: Integer = - MAXDWORD;
      const aMax: Integer = MAXDWORD; const aInc: Integer = 1);
  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; override; stdcall;
    function EndEdit: boolean; override; stdcall;

    procedure KeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
  end;

  { TVtFloatEditLink }

  TVtFloatEditLink = class(TVtBaseEditLink)
  private
    fMin: Double;
    fMax: Double;
    fInc: Double;
    fDecimals: Integer;
  public
    constructor Create(const aMin: Double = - MAXDWORD;
      const aMax: Double = MAXDWORD; const aDecimals: Integer = 5;
      const aInc: Double = 0.1);
  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; override; stdcall;
    function EndEdit: boolean; override; stdcall;

    procedure KeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
  end;

  { TVtDirectoryEditLink }

  TVtDirectoryEditLink = class(TVtBaseEditLink)
  private
    fRootDir: string;
  public
    constructor Create(const aRootDir: string = '');
  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; override; stdcall;
    function EndEdit: boolean; override; stdcall;

    procedure KeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
  end;

  { TVtFileEditLink }

  TVtFileEditLink = class(TVtBaseEditLink)
  private
    fInitialDir: string;
    fFilter: string;
  public
    constructor Create(const aInitialDir: string = ''; const aFilter: string = '');
  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; override; stdcall;
    function EndEdit: boolean; override; stdcall;

    procedure KeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
  end;

  { TVtLineEditLink }

  TVtLineEditLink = class(TVtBaseEditLink)
  private
    fImageList: TImageList;
  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; override; stdcall;
    function EndEdit: boolean; override; stdcall;
    function BeginEdit: boolean; override; stdcall;

    procedure KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CloseUp(Sender: TObject);

  end;

  { TVtMemoEditLink }

  TVtMemoEditLink = class(TVtBaseEditLink)
  protected
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; override; stdcall;
    function EndEdit: boolean; override; stdcall;

    procedure KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  end;



implementation

{ TVtBaseData }

function TVtBaseData.GetImageIndex: integer;
begin
  Result := -1;
end;

function TVtBaseData.GetSelectedIndex: integer;
begin
  Result := -1;
end;

function TVtBaseData.GetEditLink: IVtEditLink;
begin
  Result := nil;
end;

{ TVtBaseEditLink }

destructor TVtBaseEditLink.Destroy;
begin
  if fEdit <> nil then
    if fEdit.HandleAllocated then
      PostMessage(fEdit.Handle, CM_RELEASE, 0, 0);
  inherited Destroy;
end;

function TVtBaseEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
begin
  Result := Tree is TVirtualStringTree;
  if Result then
  begin
    fTree := Tree as TVirtualStringTree;
    fNode := Node;
    fColumn := Column;
  end;
end;

function TVtBaseEditLink.BeginEdit: boolean; stdcall;
begin
  Result := fEdit <> nil;
  if Result then
  begin
    fEdit.Show;
    fEdit.SetFocus;
  end;
end;

function TVtBaseEditLink.CancelEdit: boolean; stdcall;
begin
  Result := fEdit <> nil;
  if Result then
  begin
    fEdit.Hide;
    fTree.SetFocus;
  end;
end;

function TVtBaseEditLink.EndEdit: boolean; stdcall;
begin
  Result := fEdit <> nil;
  if Result then
  begin
    fEdit.Hide;
    fTree.SetFocus;
  end;
end;

function TVtBaseEditLink.GetBounds: TRect; stdcall;
begin
  if fEdit <> nil then
    Result := fEdit.BoundsRect;
end;

procedure TVtBaseEditLink.ProcessMessage(var Message: TMessage); stdcall;
begin
  if fEdit <> nil then
    fEdit.WindowProc(Message);
end;

procedure TVtBaseEditLink.SetBounds(R: TRect); stdcall;
var
  Dummy: integer;
begin
  if fEdit <> nil then
  begin
    fTree.Header.Columns.GetColumnBounds(fColumn, Dummy, R.Right);
    fEdit.BoundsRect := R;
  end;
end;


{ TVtComboEditLink }

constructor TVtComboEditLink.Create(const aCommaText: string);
begin
  inherited Create;
  fEdit := TComboBox.Create(Application);
  TComboBox(fEdit).Items.CommaText := aCommaText;
end;

constructor TVtComboEditLink.Create(const aStrings: TStrings);
begin
  inherited Create;
  fEdit := TComboBox.Create(Application);
  TComboBox(fEdit).Items.Assign(aStrings);
end;

function TVtComboEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then
  begin
    TComboBox(fEdit).Parent := fTree;
    TComboBox(fEdit).Visible := False;
    TComboBox(fEdit).Text := fTree.Text[fNode, fColumn];
    TComboBox(fEdit).DropDownCount := 16;
    TComboBox(fEdit).Style := csOwnerDrawFixed;
    TComboBox(fEdit).ReadOnly := True;
    TComboBox(fEdit).OnCloseUp := @CloseUp;
    TComboBox(fEdit).OnKeyDown := @KeyDown;
  end;
end;

function TVtComboEditLink.BeginEdit: boolean; stdcall;
begin
  Result := inherited BeginEdit;
  TComboBox(fEdit).DroppedDown := True;
end;

function TVtComboEditLink.EndEdit: boolean; stdcall;
begin
  fTree.Text[fNode, fColumn] := TComboBox(fEdit).Text;
  Result := inherited EndEdit;
end;

procedure TVtComboEditLink.KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if FEdit = nil then
    Exit;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      fTree.CancelEditNode;
    end;

    VK_RETURN:
      if Shift = [] then
      begin
        Key := 0;
        fTree.EndEditNode;
      end;

  end;
end;

procedure TVtComboEditLink.CloseUp(Sender: TObject);
begin
  fTree.EndEditNode;
end;


{ TVtIntegerEditLink }

constructor TVtIntegerEditLink.Create(const aMin: Integer; const aMax: Integer;
  const aInc: Integer);
begin
  inherited Create;
  fMin := aMin;
  fMax := aMax;
  fInc := aInc;
end;

function TVtIntegerEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then
  begin
    fEdit := TSpinEdit.Create(Application);
    TSpinEdit(fEdit).Parent := fTree;
    TSpinEdit(fEdit).Visible := False;
    TSpinEdit(fEdit).MinValue := fMin;
    TSpinEdit(fEdit).MaxValue := fMax;
    TSpinEdit(fEdit).Increment := fInc;
    TSpinEdit(fEdit).Text := fTree.Text[fNode, fColumn];
    TSpinEdit(fEdit).OnKeyDown := @KeyDown;
  end;
end;

function TVtIntegerEditLink.EndEdit: boolean; stdcall;
begin
  fTree.Text[fNode, fColumn] := TSpinEdit(fEdit).Text;
  Result := inherited EndEdit;
end;

procedure TVtIntegerEditLink.KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if FEdit = nil then
    Exit;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      fTree.CancelEditNode;
    end;

    VK_RETURN:
    begin
      Key := 0;
      fTree.EndEditNode;
    end;
  end;
end;

{ TVtFloatEditLink }

constructor TVtFloatEditLink.Create(const aMin: Double; const aMax: Double;
  const aDecimals: Integer; const aInc: Double);
begin
  inherited Create;
  fMin := aMin;
  fMax := aMax;
  fDecimals := aDecimals;
  fInc := aInc;
end;

function TVtFloatEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then
  begin
    fEdit := TFloatSpinEdit.Create(Application);
    TFloatSpinEdit(fEdit).Parent := fTree;
    TFloatSpinEdit(fEdit).Visible := False;
    TFloatSpinEdit(fEdit).MinValue := fMin;
    TFloatSpinEdit(fEdit).MaxValue := fMax;
    TFloatSpinEdit(fEdit).Increment := fInc;
    TFloatSpinEdit(fEdit).DecimalPlaces := fDecimals;
    TFloatSpinEdit(fEdit).Text := fTree.Text[fNode, fColumn];
    TFloatSpinEdit(fEdit).OnKeyDown := @KeyDown;
  end;
end;

function TVtFloatEditLink.EndEdit: boolean; stdcall;
begin
  fTree.Text[fNode, fColumn] := TFloatSpinEdit(fEdit).Text;
  Result:=inherited EndEdit;
end;

procedure TVtFloatEditLink.KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if FEdit = nil then
    Exit;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      fTree.CancelEditNode;
    end;

    VK_RETURN:
    begin
      Key := 0;
      fTree.EndEditNode;
    end;
  end;
end;


{ TVtDirectoryEditLink }

constructor TVtDirectoryEditLink.Create(const aRootDir: string);
begin
  inherited Create;
  fRootDir := aRootDir;
  if fRootDir = '' then
  	fRootDir := ExtractFileDir(ParamStr(0));
end;

function TVtDirectoryEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then
  begin
  	fEdit := TDirectoryEdit.Create(Application);
  	TDirectoryEdit(fEdit).Parent := fTree;
  	TDirectoryEdit(fEdit).Visible := False;
  	TDirectoryEdit(fEdit).OnKeyDown := @KeyDown;
    TDirectoryEdit(fEdit).RootDir := fRootDir;
    TDirectoryEdit(fEdit).FocusOnButtonClick := True;
    TDirectoryEdit(fEdit).Text := fTree.Text[fNode, fColumn];
  end;

end;

function TVtDirectoryEditLink.EndEdit: boolean; stdcall;
begin
  fTree.Text[fNode, fColumn] := TDirectoryEdit(fEdit).Text;
  Result:=inherited EndEdit;
end;

procedure TVtDirectoryEditLink.KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if FEdit = nil then
    Exit;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      fTree.CancelEditNode;
    end;

    VK_RETURN:
    begin
      Key := 0;
      fTree.EndEditNode;
    end;
  end;
end;


{ TVtFileEditLink }

constructor TVtFileEditLink.Create(const aInitialDir: string;
  const aFilter: string);
begin
  fInitialDir := aInitialDir;
  if fInitialDir = '' then
  	fInitialDir := ExtractFileDir(ParamStr(0));
  fFilter := aFilter;
  if fFilter = '' then
  	fFilter := 'All files (*.*)|*.*'
end;

function TVtFileEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then
  begin
  	fEdit := TFileNameEdit.Create(Application);
  	TFileNameEdit(fEdit).Parent := fTree;
  	TFileNameEdit(fEdit).Visible := False;
  	TFileNameEdit(fEdit).OnKeyDown := @KeyDown;
    TFileNameEdit(fEdit).InitialDir := fInitialDir;
    TFileNameEdit(fEdit).Filter := fFilter;
    TFileNameEdit(fEdit).FocusOnButtonClick := True;
    TFileNameEdit(fEdit).Text := fTree.Text[fNode, fColumn];
  end;
end;

function TVtFileEditLink.EndEdit: boolean; stdcall;
begin
  fTree.Text[fNode, fColumn] := TFileNameEdit(fEdit).Text;
  Result:=inherited EndEdit;
end;

procedure TVtFileEditLink.KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if FEdit = nil then
    Exit;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      fTree.CancelEditNode;
    end;

    VK_RETURN:
    begin
      Key := 0;
      fTree.EndEditNode;
    end;
  end;
end;


{ TVtLineEditLink }

function TVtLineEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
var
  Item: TCollectionItem;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then
  begin
  	fEdit := TComboBoxEx.Create(Application);
  	TComboBoxEx(fEdit).Parent := fTree;

  	fImageList := TImageList.Create(fEdit);
  	fImageList.Width := 120;
  	fImageList.Height := 16;

    fImageList.AddLazarusResource('SdSolid');
    fImageList.AddLazarusResource('SdDot');
    fImageList.AddLazarusResource('SdDash');
  	fImageList.AddLazarusResource('SdDashDot');
  	fImageList.AddLazarusResource('SdDashDotDot');

    TComboBoxEx(fEdit).Images := fImageList;
    TComboBoxEx(fEdit).Visible := False;

    TComboBoxEx(fEdit).ItemsEx.AddItem('Solid', 0);
    TComboBoxEx(fEdit).ItemsEx.AddItem('Dot', 1);
    TComboBoxEx(fEdit).ItemsEx.AddItem('Dash', 2);
    TComboBoxEx(fEdit).ItemsEx.AddItem('DashDot', 3);
    TComboBoxEx(fEdit).ItemsEx.AddItem('DashDotDot', 4);

    for Item in TComboBoxEx(fEdit).ItemsEx do
    	if TComboExItem(Item).Caption = fTree.Text[fNode, fColumn] then
      begin
        TComboBoxEx(fEdit).ItemIndex := TComboExItem(Item).Index;
        Break;
      end;
    TComboBoxEx(fEdit).DropDownCount := 16;
    TComboBoxEx(fEdit).ReadOnly := True;
    TComboBoxEx(fEdit).OnCloseUp := @CloseUp;
    TComboBoxEx(fEdit).OnKeyDown := @KeyDown;
  end;
end;

function TVtLineEditLink.EndEdit: boolean; stdcall;
begin
  fTree.Text[fNode, fColumn] := TComboExItem(TComboBoxEx(fEdit).ItemsEx.Items[TComboBoxEx(fEdit).ItemIndex]).Caption;
  Result:=inherited EndEdit;
end;

function TVtLineEditLink.BeginEdit: boolean; stdcall;
begin
  Result:=inherited BeginEdit;
  TComboBoxEx(fEdit).DroppedDown := True;
end;

procedure TVtLineEditLink.KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if FEdit = nil then
    Exit;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      fTree.CancelEditNode;
    end;

    VK_RETURN:
      if Shift = [] then
      begin
        Key := 0;
        fTree.EndEditNode;
      end;

  end;

end;

procedure TVtLineEditLink.CloseUp(Sender: TObject);
begin
  fTree.EndEditNode;
end;




{ TVtMemoEditLink }

function TVtMemoEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then
  begin
  	fEdit := TMemo.Create(Application);
  	TMemo(fEdit).Parent := fTree;
  	TMemo(fEdit).Visible := False;
    TMemo(fEdit).WordWrap := True;
    TMemo(fEdit).ScrollBars := ssVertical;
    TMemo(fEdit).OnKeyDown := @KeyDown;
    TMemo(fEdit).Text := fTree.Text[fNode, fColumn];
  end;
end;

function TVtMemoEditLink.EndEdit: boolean; stdcall;
begin
  fTree.Text[fNode, fColumn] := TMemo(fEdit).Text;
  Result:=inherited EndEdit;
end;

procedure TVtMemoEditLink.KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if FEdit = nil then
    Exit;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      fTree.CancelEditNode;
    end;

    VK_RETURN:
      if Shift = [] then
      begin
        Key := 0;
        fTree.EndEditNode;
      end;

  end;
end;


initialization
  {$I vteditor.lrs}
end.


