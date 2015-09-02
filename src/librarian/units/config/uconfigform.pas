unit uConfigForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,
  uLibraryData,
  uregisterseditor,
  uConfigEditor;

type

  { TConfigForm }

  TConfigForm = class(TEditingForm)
    ConfigImageList: TImageList;
    Splitter: TSplitter;
    RegistersPanel: TPanel;
    ConfigPanel: TPanel;
    procedure FormCreate(Sender: TObject);

  private
    fRegistersEditor: TRegistersEditor;
    fConfigEditor: TConfigEditor;
  public
    procedure Load(const aLibraryData: TLibraryData); override;
    procedure Unload; override;
  end;


implementation

{$R *.lfm}

{ TConfigForm }

procedure TConfigForm.FormCreate(Sender: TObject);
begin
	fRegistersEditor := TRegistersEditor.Create(Self);
  fRegistersEditor.Parent := RegistersPanel;
  fRegistersEditor.Align := alClient;

  fConfigEditor := TConfigEditor.Create(Self);
  fConfigEditor.Parent := ConfigPanel;
  fConfigEditor.Align := alClient;
  fConfigEditor.Images := ConfigImageList;

end;

procedure TConfigForm.Load(const aLibraryData: TLibraryData);
begin
	if aLibraryData is TConfigurationData then
  begin
  	fRegistersEditor.LoadRegisters(TConfigurationData(aLibraryData).Registers);
  	fConfigEditor.LoadConfig(TConfigurationData(aLibraryData).Configuration);
  end;
end;

procedure TConfigForm.Unload;
begin
	fRegistersEditor.Clear;

  fConfigEditor.Clear;
end;

end.

