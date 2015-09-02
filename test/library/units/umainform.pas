unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ulibrary;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    S: String;
    fLibrary: ILibrary;
    V: IVarDefine;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fLibrary := GetLibrary(['library\Developer\module.jlf', 'library\User\module.jlf']);
  V := fLibrary[slDeveloper][1].ModuleDefine.Registers[TTypeRegister.trHolding]['0E64E3ED-500F-4951-8836-F68D6CF3005E'];
  Edit1.Text := {Utf8EnCode}(V.ShortDescription);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  V.ShortDescription := {AnsiToUtf8}(Edit1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Edit1.Text := {Utf8ToAnsi}(V.ShortDescription);
end;

end.

