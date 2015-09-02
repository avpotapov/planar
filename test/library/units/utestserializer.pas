unit uTestSerializer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uLibrary,
  uSetting;

type

  { TTestSerializer }

  TTestSerializer= class(TTestCase)
  private
    fLibrary: ILibrary;
  protected
    procedure SetUp; override;
  published
    procedure TestSerialize;
  end;

implementation

procedure TTestSerializer.SetUp;
var
	DeveloperModule, UserModule: string;
begin
  DeveloperModule := GetSetting.DeveloperLibrary + '\module.jlf';
  UserModule := GetSetting.UserLibrary + '\module.jlf';
  fLibrary := GetLibrary([DeveloperModule, UserModule]);
  AssertTrue(fLibrary <> nil);
end;

procedure TTestSerializer.TestSerialize;
begin
	SaveLibrary(fLibrary);
  AssertTrue(FileExists(GetSetting.DeveloperLibrary + '\module.bak'));
  AssertTrue(FileExists(GetSetting.DeveloperLibrary + '\module.jlf'));
//  AssertTrue(FileExists(GetSetting.UserLibrary + '\module.bak'));
//  AssertTrue(FileExists(GetSetting.UserLibrary + '\module.jlf'));
end;



initialization

  RegisterTest(TTestSerializer);
end.

