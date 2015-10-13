unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, lclintf, StdCtrls,
  ValEdit, ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseEnter(Sender: TObject);
    procedure Image1MouseLeave(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses uAbout;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Label1.Caption := Label1.Caption + ' ' +  AnsiToUtf8(ConfiguratorVersion.VersionStrings[8]);
  Label2.Caption := Label2.Caption + '  ' +  AnsiToUtf8(LibrarianVersion.VersionStrings[8]);


   {
  for	I := 0 to 	ConfiguratorVersion.VersionStrings.Count - 1 do
     VersionValueListEditor.Strings.Add (Format ('%s=%s', [AnsiToUtf8(ConfiguratorVersion.VersionCategories[I]),	AnsiToUtf8(ConfiguratorVersion.VersionStrings[I])]));
  }
//   VersionValueListEditor.Strings.Add(Format('Приложение=%s', [AnsiToUtf8(ConfiguratorVersion.VersionStrings[7])]));
 //  VersionValueListEditor.Strings.Add(Format('Версия приложения=%s', [AnsiToUtf8(ConfiguratorVersion.VersionStrings[8])]));
 //  VersionValueListEditor.Strings.Add(Format('Версия библиотеки=%s', [AnsiToUtf8(LibrarianVersion.VersionStrings[8])]));

end;

procedure TAboutForm.Image1Click(Sender: TObject);
begin
   OpenURL('http://jetlogic.ru/');
end;

procedure TAboutForm.Image1MouseEnter(Sender: TObject);
begin
	Screen.Cursor:= crHandPoint;
end;

procedure TAboutForm.Image1MouseLeave(Sender: TObject);
begin
	Screen.Cursor:= crDefault;
end;



end.

