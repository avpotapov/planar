unit ucustomeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, VirtualTrees, Windows;

type
{$REGION Пользовательские редакторы}

  { TBaseData }

  TBaseData = class
  type
    TTypeEditor = (
                    teNone,
                    teFloatSpinEdit,
                    teSpinEdit,
                    teEdit,
                    teDirectoryEdit,
                    teComboBox,
                    teMemo
                  );
  protected
    fTypeEditor: TTypeEditor;
  public
    constructor Create(const aTypeEditor: TTypeEditor = TTypeEditor.teNone); reintroduce;
  public
    property TypeEditor: TTypeEditor read fTypeEditor write fTypeEditor;
  end;

  { TBaseEditorFactory }

  TBaseEditorFactory = class
  protected
    fTree:      TBaseVirtualTree;
    fNode:      PVirtualNode;
    fColumn:    Integer;
    fOnKeyDown: TKeyEvent;
    fOnKeyUp:   TKeyEvent;
    fOnChange:   TNotifyEvent;
    fEditor:    TWinControl;

  protected
    procedure ConstructEditor; virtual; abstract;
    // Редакторы
    procedure ConstructComponent(const aComponentClass: TComponentClass);

  public
    {%H-}constructor Create (
                              const aTree: TBaseVirtualTree;
                              const aNode: PVirtualNode;
                              const aColumn: Integer;
                              const aOnKeyDown, aOnKeyUp: TKeyEvent;
                              const aOnChange: TNotifyEvent
                            ); reintroduce;
  end;

  { TBaseEditor }

  TBaseEditor = class(TWinControl, IVTEditLink)
  protected
    fEditor: TWinControl; // Редактор значения - создается динамически
    fTree:   TVirtualStringTree;
    fNode:   PVirtualNode;
    fColumn: Integer;

  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure OnChange(Sender: TObject);

  public
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall; virtual; 
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): Boolean; stdcall; virtual; 
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); reintroduce; stdcall;
  end;

  {$ENDREGION Пользовательские редакторы}
implementation

{$REGION Пользовательские редакторы}

{ TBaseData }

constructor TBaseData.Create(const aTypeEditor: TTypeEditor);
begin
  inherited Create;
  fTypeEditor := aTypeEditor;
end;


{ TBaseEditorFactory }

constructor TBaseEditorFactory.Create(const aTree: TBaseVirtualTree;
  const aNode: PVirtualNode; const aColumn: Integer; const aOnKeyDown, aOnKeyUp: TKeyEvent;
  const aOnChange: TNotifyEvent);
begin
  inherited Create;
  fTree      := aTree;
  fNode      := aNode;
  fColumn    := aColumn;
  fOnKeyDown := aOnKeyDown;
  fOnKeyUp   := aOnKeyUp;
  fOnChange  := aOnChange;
end;

procedure TBaseEditorFactory.ConstructComponent(const aComponentClass: TComponentClass);
begin
  fEditor := aComponentClass.Create(Application) as TWinControl;
  fEditor.Parent    := fTree;
  fEditor.Visible   := False;
  fEditor.OnKeyDown := fOnKeyDown;
  fEditor.OnKeyUp   := fOnKeyUp;
end;


{ TBaseEditor }

function TBaseEditor.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean; stdcall;
begin
  Result := True;
  fTree   := Tree as TVirtualStringTree;
  fNode   := Node;
  fColumn := Column;
end;

function TBaseEditor.BeginEdit: Boolean; stdcall;

begin
  Result := False;
  if fEditor <> nil then
  begin
    fEditor.Show;
    fEditor.SetFocus;
    if fEditor is TComboBox then
      (fEditor as TComboBox).DroppedDown := True;
    Result := True;
  end;
end;

function TBaseEditor.CancelEdit: Boolean; stdcall;
begin
  Result := False;
  if fEditor <> nil then
  begin
    fEditor.Hide;
    Result := True;
  end;
end;

function TBaseEditor.EndEdit: Boolean; stdcall;
begin

  fEditor.Hide;
  fTree.SetFocus;
  Result := True;

end;

function TBaseEditor.GetBounds: TRect; stdcall;
begin
  if fEditor = nil then
    Exit;
  Result := fEditor.BoundsRect;
end;


procedure TBaseEditor.ProcessMessage(var Message: TMessage); stdcall;
begin
  if fEditor = nil then
    Exit;
  fEditor.WindowProc(Message);
end;

procedure TBaseEditor.SetBounds(R: TRect); stdcall;
var
  Dummy: Integer;
begin
  if fEditor = nil then
    Exit;
  fTree.Header.Columns.GetColumnBounds(fColumn, Dummy, R.Right);
  fEditor.BoundsRect := R;
end;

procedure TBaseEditor.OnChange(Sender: TObject);
begin
  if fEditor is TComboBox then
  begin
    fTree.EndEditNode;
    fTree.SetFocus;
  end;
end;

procedure TBaseEditor.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  if FEditor = nil then
    Exit;

  CanAdvance := True;
  case Key of

    VK_ESCAPE:
    begin
      Key := 0; // ESC
    end;

    VK_RETURN:
    begin
      CanAdvance := Shift = [];
      if CanAdvance then
      begin
        fTree.EndEditNode;
        Key := 0;
      end;
    end;

    VK_UP, VK_DOWN:
    begin

      CanAdvance := Shift = [];

      if fEditor is TComboBox then
        CanAdvance := False;    //CanAdvance and not TComboBox(FEditor).DroppedDown;

      if fEditor is TMemo then
        CanAdvance := False;

      if CanAdvance then
      begin
        PostMessage(fTree.Handle, WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    end;
  end;

end;

procedure TBaseEditor.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
    begin
      fTree.CancelEditNode;
      Key := 0;
    end;
  end;
end;

destructor TBaseEditor.Destroy;
begin
  if fEditor <> nil then
    if fEditor.HandleAllocated then
      PostMessage(fEditor.Handle, CM_RELEASE, 0, 0);
  inherited Destroy;
end;

{$ENDREGION Пользовательские редакторы}
end.

