unit uSplashForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uAbout;

type

  { TSplashForm }

  TSplashForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Logo: TImage;
    procedure FormCreate(Sender: TObject);
  private
    fLogoFileName: string;
  public
    constructor Create(const aLogoFileName: string); reintroduce;
  end;


implementation

{$R *.lfm}


{ TSplashForm }

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  if FileExists(fLogoFileName) then
  Logo.Picture.Png.LoadFromFile(fLogoFileName);
  Label1.Caption := Label1.Caption + ' ' +  AnsiToUtf8(ConfiguratorVersion.VersionStrings[8]);
  Label2.Caption := Label2.Caption + '  ' +  AnsiToUtf8(LibrarianVersion.VersionStrings[8]);

end;

constructor TSplashForm.Create(const aLogoFileName: string);
begin
  inherited Create(Application);
  fLogoFilename := aLogoFilename;
end;

end.

