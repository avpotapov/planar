unit uSplashForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TSplashForm }

  TSplashForm = class(TForm)
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
end;

constructor TSplashForm.Create(const aLogoFileName: string);
begin
  inherited Create(Application);
  fLogoFilename := aLogoFilename;
end;

end.

