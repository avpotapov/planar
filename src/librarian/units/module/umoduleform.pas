unit uModuleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, uLibraryData,
  uModuleData;

type

  { TModuleForm }

  TModuleForm = class(TEditingForm)
    procedure FormCreate(Sender: TObject);
  private
  	fModuleEditor: TModuleEditor;
  public
    procedure Load(const aLibraryData: TLibraryData); override;
    procedure Unload; override;
  end;

implementation


{$R *.lfm}

{ TModuleForm }

procedure TModuleForm.FormCreate(Sender: TObject);
begin
	fModuleEditor := TModuleEditor.Create(Self);
  fModuleEditor.Parent := Self;
  fModuleEditor.Align := alClient;
end;

procedure TModuleForm.Load(const aLibraryData: TLibraryData);
begin
	if aLibraryData is uLibraryData.TModuleData then
  	fModuleEditor.Module := uLibraryData.TModuleData(aLibraryData).Module;
end;

procedure TModuleForm.Unload;
begin
	fModuleEditor.Clear;
end;

end.

