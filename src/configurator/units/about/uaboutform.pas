unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, lclintf, StdCtrls,
  ValEdit;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    StaticText: TStaticText;
    VersionValueListEditor: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure StaticTextClick(Sender: TObject);
    procedure StaticTextMouseEnter(Sender: TObject);
    procedure StaticTextMouseLeave(Sender: TObject);
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
var
	I: Integer;
begin
  VersionValueListEditor.Strings.Clear;
  {
  for	I := 0 to 	ConfiguratorVersion.VersionStrings.Count - 1 do
     VersionValueListEditor.Strings.Add (Format ('%s=%s', [AnsiToUtf8(ConfiguratorVersion.VersionCategories[I]),	AnsiToUtf8(ConfiguratorVersion.VersionStrings[I])]));
  }
//   VersionValueListEditor.Strings.Add(Format('Приложение=%s', [AnsiToUtf8(ConfiguratorVersion.VersionStrings[7])]));
   VersionValueListEditor.Strings.Add(Format('Версия приложения=%s', [AnsiToUtf8(ConfiguratorVersion.VersionStrings[8])]));
   VersionValueListEditor.Strings.Add(Format('Версия библиотеки=%s', [AnsiToUtf8(LibrarianVersion.VersionStrings[8])]));

end;

procedure TAboutForm.StaticTextClick(Sender: TObject);
begin
    OpenURL('http://planar-smt.ru/');
end;

procedure TAboutForm.StaticTextMouseEnter(Sender: TObject);
begin
  StaticText.Cursor := crHandPoint;
  {cursor changes into handshape when it is on StaticText}
  StaticText.Font.Color := clHotLight;
  {StaticText changes color into blue when cursor is on StaticText}
end;

procedure TAboutForm.StaticTextMouseLeave(Sender: TObject);
begin
  StaticText.Font.Color := clDefault;
end;

end.

