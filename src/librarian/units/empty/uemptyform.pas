unit uEmptyForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, uLibraryData;

type

  { TEmptyForm }

  TEmptyForm = class(TEditingForm)
  private
    { private declarations }
  public
    procedure Load(const {%H-}aLibraryData: TLibraryData); override;
    procedure Unload; override;
  end;

var
  EmptyForm: TEmptyForm;

implementation

{$R *.lfm}

{ TEmptyForm }

procedure TEmptyForm.Load(const aLibraryData : TLibraryData);
begin

end;

procedure TEmptyForm.Unload;
begin

end;

end.

