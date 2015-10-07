unit uUpdateInfoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lclintf, ExtCtrls;

type

  { TUpdateInfoForm }

  TUpdateInfoForm = class(TForm)
    DescMemo: TMemo;
    FileLinkStatText: TStaticText;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    VersionLabeledEdit: TLabeledEdit;
    procedure FileLinkStatTextClick(Sender: TObject);
    procedure FileLinkStatTextMouseEnter(Sender: TObject);
    procedure FileLinkStatTextMouseLeave(Sender: TObject);
  private
    procedure SetDescription(AValue: TStringList);
    procedure SetFileLink(AValue: string);
    procedure SetVersion(AValue: string);
    { private declarations }
  public
    property Version: string write SetVersion;
    property FileLink: string write SetFileLink;
    property DEscription: TStringList write SetDescription;

  end;


implementation

{$R *.lfm}

{ TUpdateInfoForm }

procedure TUpdateInfoForm.FileLinkStatTextClick(Sender: TObject);
begin
  OpenURL(FileLinkStatText.Caption);
end;

procedure TUpdateInfoForm.FileLinkStatTextMouseEnter(Sender: TObject);
begin
  FileLinkStatText.Cursor := crHandPoint;
  {cursor changes into handshape when it is on StaticText}
  FileLinkStatText.Font.Color := clBlue;
  {StaticText changes color into blue when cursor is on StaticText}
end;

procedure TUpdateInfoForm.FileLinkStatTextMouseLeave(Sender: TObject);
begin
  FileLinkStatText.Font.Color := clDefault;
  {when cursor is not on StaticText then color of text changes into default color}
end;

procedure TUpdateInfoForm.SetDescription(AValue: TStringList);
begin
  DescMemo.Lines.Assign(AValue);
end;

procedure TUpdateInfoForm.SetFileLink(AValue: string);
begin
  FileLinkStatText.Caption := AValue;
end;

procedure TUpdateInfoForm.SetVersion(AValue: string);
begin
 VersionLabeledEdit.Text := AValue
end;

end.

