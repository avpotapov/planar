unit uUpdateForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Windows;

type

  { TUpdateForm }

  TUpdateForm = class(TForm)
    OnePerWeekCheckBox: TCheckBox;
    EveryStartApplicationCheckBox: TCheckBox;
    CheckUpdateButton: TButton;
    CloseButton: TButton;
    procedure CheckUpdateButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

uses uCheckUpdate, uUpdateInfoForm, uAbout;

{$R *.lfm}

{ TUpdateForm }

procedure TUpdateForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TUpdateForm.CheckUpdateButtonClick(Sender: TObject);
var
	Upd: TUpdate;
begin
	if TUpdate.GetUpdateFile then
  begin
    Upd := TUpdate.Create;
    Upd.Parse();
    if uAbout.Version.VersionStrings[8] = Upd.UpdateSoft[0].Version then
    begin
      MessageBox(Handle, PChar(Utf8ToAnsi('Обновление не требуется')),
          PChar(Utf8ToAnsi('Информация')), MB_ICONWARNING + MB_OK);
     Exit;
    end;
    with TUpdateInfoForm.Create(Self) do
		try
      Description := Upd.UpdateSoft[0].Description;
      FileLink :=    Upd.UpdateSoft[0].FileLink;
      Version :=    Upd.UpdateSoft[0].Version;
      ShowModal;

    finally
      Free;
    end;

  end
  else
    MessageBox(Handle, PChar(Utf8ToAnsi('Ошибка получения обновления')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
end;

end.

