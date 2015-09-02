unit uBitsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, VirtualTrees,
  uLibrary,
  uLibraryData,
  uBitsEditor,
  uBitEditor;

type

  { TBitsForm }

  TBitsForm = class(TEditingForm)
    BitPanel: TPanel;
    BitsPanel: TPanel;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);

  private
    fBitsEditor: TBitsEditor;
    fBitEditor: TBitEditor;

  private
    procedure BitsSelect(Sender: TObject; const aBits: IBits);
  public
    procedure Load(const aLibraryData : TLibraryData); override;
    procedure Unload; override;
  end;

implementation


{$R *.lfm}


{ TBitsForm }

procedure TBitsForm.FormCreate(Sender: TObject);
begin
  fBitsEditor := TBitsEditor.Create(Self);
  fBitsEditor.Parent := BitsPanel;
  fBitsEditor.Align := alClient;
  fBitsEditor.OnBitsSelect := @BitsSelect;

  fBitEditor := TBitEditor.Create(Self);
  fBitEditor.Parent := BitPanel;
  fBitEditor.Align := alClient;
end;

procedure TBitsForm.BitsSelect(Sender: TObject; const aBits: IBits);
begin
  fBitEditor.Bits := aBits;
end;

procedure TBitsForm.Load(const aLibraryData: TLibraryData);
begin
  if aLibraryData is uLibraryData.TBitsData then
  	fBitsEditor.BitsSet := uLibraryData.TBitsData(aLibraryData).BitsSet;
end;

procedure TBitsForm.Unload;
begin
  fBitsEditor.Clear;
  fBitEditor.Clear;
end;

end.

