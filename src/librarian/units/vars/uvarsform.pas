unit uvarsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, VirtualTrees,
  uLibrary,
  uLibraryData,
  uVarsEditor,
  uVarEditor,
  uBitEditor,
  uPickItemEditor;

type

  { TVarsForm }

  TVarsForm = class(TEditingForm)
    PresetsBox: TComboBox;
    ImageList: TImageList;
    PageControl: TPageControl;
    BasicTabSheet: TTabSheet;
    ExtraTabSheet: TTabSheet;
    ExtraToolBar: TToolBar;
    ClearButton: TToolButton;
    AddButton: TToolButton;
    VarsPanel: TPanel;
    VarPanel: TPanel;
    Splitter: TSplitter;
    procedure AddButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PresetsBoxDropDown(Sender: TObject);

  private
    fVarsEditor: TVarsEditor;
    fVarEditor: TVarEditor;
    fBitEditor: TBitEditor;
    fPickItemEditor: TPickItemEditor;

    fBitsSet: TStrings;
    fPickLists: TStrings;
    fCurrentVar: IVarDefine;

  private
    procedure VarSelect(Sender: TObject; const aVar: IVarDefine);
    procedure LoadPresets(const aLibraryData: TLibraryData);
  public
    procedure Load(const aLibraryData: TLibraryData); override;
    procedure Unload; override;
  end;



implementation


{$R *.lfm}

procedure TVarsForm.FormCreate(Sender: TObject);
begin
  fVarsEditor := TVarsEditor.Create(Self);
  fVarsEditor.Parent := VarsPanel;
  fVarsEditor.Align := alClient;
  fVarsEditor.OnVarSelect := @VarSelect;

  fVarEditor := TVarEditor.Create(Self);
  fVarEditor.Parent := BasicTabSheet;
  fVarEditor.Align := alClient;

  fBitEditor := TBitEditor.Create(Self);
  fBitEditor.Parent := ExtraTabSheet;
  fBitEditor.Align := alClient;
  fBitEditor.Visible := False;

  fPickItemEditor := TPickItemEditor.Create(Self);
  fPickItemEditor.Parent := ExtraTabSheet;
  fPickItemEditor.Align := alClient;
  fPickItemEditor.Visible := False;

  fBitsSet := TStringList.Create;
  fPickLists := TStringList.Create;

  PresetsBox.Items.Add('создать');
  PresetsBox.ItemIndex := 0;
end;

procedure TVarsForm.AddButtonClick(Sender: TObject);
begin
  if fCurrentVar <> nil then
  begin
    case fCurrentVar.VarType of
      vtBITMAP16, vtBITMAP32:
      begin
        if PresetsBox.ItemIndex > 0 then
        	fCurrentVar.Bits := IBits(Pointer(PresetsBox.Items.Objects[PresetsBox.ItemIndex]));

        fBitEditor.Bits := fCurrentVar.Bits;
        fBitEditor.Visible := True;
      end;
      else
      begin
         if PresetsBox.ItemIndex > 0 then
        	fCurrentVar.PickList := IPickList(Pointer(PresetsBox.Items.Objects[PresetsBox.ItemIndex]));

        fPickItemEditor.PickList := fCurrentVar.Picklist;
        fPickItemEditor.Visible := True;
      end;
    end;
  end;
end;

procedure TVarsForm.ClearButtonClick(Sender: TObject);
begin
  if fCurrentVar <> nil then
  begin
    case fCurrentVar.VarType of
      vtBITMAP16, vtBITMAP32:
      begin
        fBitEditor.Clear;
        fCurrentVar.Bits := nil;
        fBitEditor.Visible := False;
      end;
      else
      begin
        fPickItemEditor.Clear;
        fCurrentVar.PickList := nil;
        fPickItemEditor.Visible := False;
      end;
    end;
  end;
end;

procedure TVarsForm.PresetsBoxDropDown(Sender: TObject);
begin
  if fCurrentVar <> nil then
  begin
    PresetsBox.Items.Clear;
    case fCurrentVar.VarType of
      vtBITMAP16, vtBITMAP32:
        TComboBox(Sender).Items.Assign(fBitsSet);
      else
        TComboBox(Sender).Items.Assign(fPickLists);
    end;
    PresetsBox.ItemIndex := 0;
  end;
end;


procedure TVarsForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fBitsSet);
  FreeAndNil(fPickLists);
end;



procedure TVarsForm.VarSelect(Sender: TObject; const aVar: IVarDefine);
begin
  PresetsBox.ItemIndex:= 0;
  fCurrentVar := aVar;

  fVarEditor.VarDefine := aVar;

  fBitEditor.Clear;
  fBitEditor.Visible := False;

  fPickItemEditor.Clear;
  fPickItemEditor.Visible := False;


  if aVar.Picklist.Count > 0 then
  begin
    fPickItemEditor.PickList := aVar.Picklist;
    fPickItemEditor.Visible := True;
  end
  else
  if aVar.Bits.Count > 0 then
  begin
    fBitEditor.Bits := aVar.Bits;
    fBitEditor.Visible := True;
  end;
end;

type

  { TBaseVirtualTreeHelper }

  TBaseVirtualTreeHelper = class Helper for TBaseVirtualTree
  private
    function GetData(const aNode: PVirtualNode): TLibraryData;
  end;


function TBaseVirtualTreeHelper.GetData(const aNode: PVirtualNode): TLibraryData;
var
  P: Pointer;
begin
  Result := nil;
  if aNode = nil then
    Exit;
  P := GetNodeData(aNode);
  Result := TLibraryData(P^);
end;


procedure TVarsForm.LoadPresets(const aLibraryData: TLibraryData);
var
  PresetsData: TLibraryData;
  P: Pointer;
begin
  fBitsSet.Clear;
  fPickLists.Clear;
  PresetsData := aLibraryData.Tree.GetData(
    aLibraryData.OwnerNode^.Parent^.Parent^.FirstChild^.NextSibling);
  if PresetsData is TPresetsData then
  begin
    fBitsSet.Add('создать');
    fPickLists.Add('создать');

    for P in TPresetsData(PresetsData).Presets.BitsSet do
    	fBitsSet.AddObject(
      	TPresetsData(PresetsData).Presets.BitsSet.ExtractKey(P),
        TObject(Pointer(TPresetsData(PresetsData).Presets.BitsSet.ExtractData(P)))
        );

      for P in TPresetsData(PresetsData).Presets.PickLists do
    	fPickLists.AddObject(
      	TPresetsData(PresetsData).Presets.PickLists.ExtractKey(P),
        TObject(Pointer(TPresetsData(PresetsData).Presets.PickLists.ExtractData(P)))
        );
  end;
end;



procedure TVarsForm.Load(const aLibraryData: TLibraryData);
begin
  if aLibraryData is TVarsData then
  begin
    fVarsEditor.Vars := TVarsData(aLibraryData).Vars;
    LoadPresets(aLibraryData);
  end;
end;

procedure TVarsForm.Unload;
begin
  fVarsEditor.Clear;
  fVarEditor.Clear;

  fBitEditor.Clear;
  fBitEditor.Visible := False;

  fPickItemEditor.Clear;
  fPickItemEditor.Visible := False;

  fBitsSet.Clear;

  PresetsBox.Items.Clear;
  fCurrentVar := nil;
end;

end.



