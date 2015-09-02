unit uDeviceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Windows,
  ulibrary, usetting;

type

  { TDeviceForm }

  TDeviceForm = class(TForm)
    CancelButton: TButton;
    SlaveIdBox:   TComboBox;
    ModuleBox:    TComboBox;
    SlaveIdLabel: TLabel;
    OkButton:     TButton;
    ModuleLabel:  TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);


  private
    function GetModule: IModule;
    function GetSlaveId: byte;
    procedure PopulateModuleBox;
    procedure PopulateSlaveId;
  public
    property SlaveId: byte read GetSlaveId;
    property Module: IModule read GetModule;
  end;



implementation

{$R *.lfm}

{ TDeviceForm }

procedure TDeviceForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;
end;

procedure TDeviceForm.FormCreate(Sender: TObject);
begin
  PopulateModuleBox;
  PopulateSlaveId;
end;

function TDeviceForm.GetModule: IModule;
begin
  Result := IModule(Pointer(ModuleBox.Items.Objects[ModuleBox.ItemIndex]));
end;

function TDeviceForm.GetSlaveId: byte;
begin
  Result := Integer(SlaveIdBox.Items.Objects[SlaveIdBox.ItemIndex]);
end;


procedure TDeviceForm.PopulateModuleBox;
var
  Lib: ILibrary;
  Sublib: ISublibrary;
  M: IModule;
  P, P1: Pointer;
begin
  try

    ModuleBox.Items.Add('auto');
    ModuleBox.ItemIndex := 0;

    // Загрузить библиотеку
    Lib := GetLibrary([
      GetSetting.DeveloperLibrary + '\module.jlf',
      GetSetting.UserLibrary + '\module.jlf']);

    for P in Lib do
    begin
      Sublib := Lib.ExtractData(P);
      for P1 in Sublib do
      begin
         M := Sublib.ExtractData(P1);
         ModuleBox.Items.AddObject(M.Name, TObject(Pointer(M)));
      end;
    end;

  except
      MessageBox(Handle, PChar(Utf8ToAnsi('Ошибка загрузки списка модулей')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure TDeviceForm.PopulateSlaveId;
var
  I: Integer;
begin
  for I := 1 to 247 do
     SlaveIdBox.Items.AddObject(IntToStr(I), TObject(I));
  SlaveIdBox.ItemIndex := 0;
end;

end.


