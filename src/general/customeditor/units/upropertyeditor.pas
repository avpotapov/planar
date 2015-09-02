unit upropertyeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, Spin, VirtualTrees, Windows, 
  ucustomeditor;

type
{$REGION Редактор свойств}

  { TPropertyData }

  TPropertyData = class(TBaseData)
  private
    fCaption:    String;
    fStrings: TStrings;
  protected
    function GetValue: string; virtual; abstract;
    procedure SetValue(const aValue: string); virtual; abstract;
  public
    constructor Create (
                        const aCaption: String;
                        const aTypeEditor: TTypeEditor = TTypeEditor.teNone
                       ); reintroduce;
    destructor Destroy; override;
  public
    property Caption: String read fCaption write fCaption;
    property Value: string read GetValue write SetValue;
    property Strings: TStrings read fStrings write fStrings;
  end;

  { TPropertyEditorFactory }

  TPropertyEditorFactory = class(TBaseEditorFactory)
  protected
    procedure ConstructEditor; override;

  public
    class function GetEditor (
                              const aTree: TBaseVirtualTree;
                              const aNode: PVirtualNode; 
                              const aOnKeyDown, aOnKeyUp: TKeyEvent;
                              const aOnChange: TNotifyEvent
                             ): TWinControl;
  end;

  { TPropertyEditor }

  TPropertyEditor = class(TBaseEditor)
  public
    function EndEdit: Boolean; stdcall; override;
    function PrepareEdit (
                          Tree: TBaseVirtualTree; 
                          Node: PVirtualNode;
                          Column: TColumnIndex
                         ): Boolean; stdcall; override;
  end;

  {$ENDREGION Редактор свойств}
implementation

{$REGION Редактор свойств}

{ TPropertyData }

constructor TPropertyData.Create(const aCaption: String;
  const aTypeEditor: TTypeEditor);
begin
  inherited Create;
  fStrings := TStringList.Create;
  fCaption    := aCaption;
  fTypeEditor := aTypeEditor;
end;

destructor TPropertyData.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;


{ TPropertyEditorFactory }

procedure TPropertyEditorFactory.ConstructEditor;
var
  P: Pointer;
begin
  P := fTree.GetNodeData(fNode);
  case TPropertyData(P^).TypeEditor of

    teComboBox:
    begin
      ConstructComponent(TComboBox);
      if fEditor is TComboBox then
      begin
        TComboBox(fEditor).Items.Assign(TPropertyData(P^).fStrings);
        TComboBox(fEditor).DropDownCount := 16;
        TComboBox(fEditor).Style := csOwnerDrawFixed;
        TComboBox(fEditor).ReadOnly := True;
        TComboBox(fEditor).OnChange := fOnChange;
       end;
    end;

    teSpinEdit:
    begin
      ConstructComponent(TSpinEdit);
      if fEditor is TSpinEdit then
      begin
        TSpinEdit(fEditor).MinValue  := -MAXWORD;
        TSpinEdit(fEditor).MaxValue  := MAXWORD;
        TSpinEdit(fEditor).Increment := 1;
      end;
    end;

    teFloatSpinEdit: ConstructComponent(TFloatSpinEdit);

    teEdit: ConstructComponent(TEdit);

    teDirectoryEdit:
    begin
      // Лучше сделать отдельно
      fEditor := TDirectoryEdit.Create(Application);
      TDirectoryEdit(fEditor).Parent := fTree;
      TDirectoryEdit(fEditor).Visible := False;
      TDirectoryEdit(fEditor).OnKeyDown := fOnKeyDown;
      TDirectoryEdit(fEditor).OnKeyUp := fOnKeyUp;
      TDirectoryEdit(fEditor).Text := TPropertyData(P^).Value;
    end;

    teMemo:
      begin
        ConstructComponent(TMemo);
        TMemo(fEditor).ScrollBars := ssVertical;
      end;

  end;
  TCustomEdit(fEditor).Text := TPropertyData(P^).Value;
end;


class function TPropertyEditorFactory.GetEditor(const aTree: TBaseVirtualTree;
  const aNode: PVirtualNode; const aOnKeyDown, aOnKeyUp: TKeyEvent;
  const aOnChange: TNotifyEvent): TWinControl;
begin
  with TPropertyEditorFactory.Create(aTree, aNode, -1, aOnKeyDown, aOnKeyUp, aOnChange) do
    try
      ConstructEditor;
      Result := fEditor;
    finally
      Free;
    end;
end;

{ TCustomEditor }

function TPropertyEditor.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  try
    fEditor := TPropertyEditorFactory.GetEditor (
                                                  fTree,
                                                  fNode,
                                                  @EditKeyDown,
                                                  @EditKeyUp,
                                                  @OnChange
                                                );
  except
    Result := False;
  end;
end;

function TPropertyEditor.EndEdit: Boolean; stdcall;
var
  P:      Pointer;
  Buffer: array [0 .. 1024] of Char;
  S:      string;
begin

    if fEditor = nil then
      Exit;

    GetWindowText(fEditor.Handle, Buffer, 1024);
    S := Buffer;

    // Не работает GetWindowText
    if fEditor is TDirectoryEdit then
      S := TDirectoryEdit(fEditor).Text;

    if fEditor is TComboBox then
      S := TComboBox(fEditor).Text;

    if fEditor is TEdit then
      S := TEdit(fEditor).Text;

    if fEditor is TMemo then
      S := TMemo(fEditor).Lines.Text;

    P := fTree.GetNodeData(fNode);
    if TPropertyData(P^).Value <> S then
    begin
      TPropertyData(P^).Value := S;
    end;

    fTree.InvalidateNode(FNode);

  Result := inherited EndEdit;

end;

{$ENDREGION Редактор свойств}
end.

