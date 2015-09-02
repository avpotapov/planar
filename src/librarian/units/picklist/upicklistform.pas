unit uPickListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  ExtCtrls, VirtualTrees,
  uLibraryData,
  uPickListEditor,
  uPickItemEditor,
  uLibrary;

type

  { TPickListForm }

  TPickListForm = class (TEditingForm)
    PickListPanel: TPanel;
    PickItemPanel: TPanel;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);

  private
    fPickListEditor: TPickListEditor;
    fPickItemEditor: TPickItemEditor;

  private
    procedure PickListSelect(Sender: TObject; const aPickList: IPickList);

  public
    procedure Load(const aLibraryData: TLibraryData); override;
    procedure Unload; override;
  end;


implementation

{$R *.lfm}

procedure TPickListForm.FormCreate(Sender: TObject);
begin
	 fPickListEditor := TPickListEditor.Create(Self);
   fPickListEditor.Parent := PickListPanel;
   fPickListEditor.Align := alClient;
   fPickListEditor.OnPickListSelect := @PickListSelect;

   fPickItemEditor := TPickItemEditor.Create(Self);
   fPickItemEditor.Parent := PickItemPanel;
   fPickItemEditor.Align := alClient;

end;

procedure TPickListForm.PickListSelect(Sender: TObject;
  const aPickList: IPickList);
begin
  fPickItemEditor.PickList := aPickList;
end;

procedure TPickListForm.Load(const aLibraryData: TLibraryData);
begin
  if aLibraryData is TPickListsData then
  	fPickListEditor.PickLists := TPickListsData(aLibraryData).PickLists;
end;

procedure TPickListForm.Unload;
begin
  fPickListEditor.Clear;
  fPickItemEditor.Clear;
end;

end.
