unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ValEdit;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    VersionValueListEditor: TValueListEditor;
    procedure FormCreate(Sender: TObject);
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
  for	I := 0 to 	Version.VersionStrings.Count - 1 do
     VersionValueListEditor.Strings.Add (Format ('%s=%s', [AnsiToUtf8(Version.VersionCategories[I]),	AnsiToUtf8(Version.VersionStrings[I])]));
end;

end.

