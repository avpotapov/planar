unit utestsetting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usetting, uSettingCls, fpcunit, testregistry;

type

  { TTestSetting }

  TTestSetting= class(TTestCase)
  private
    fSetting: ISetting;
  protected
    procedure SetUp; override;
  published
    procedure TestInstantiation;
    procedure TestWriteKeepAliveTimeout;
    procedure TestBaudRate;
    procedure TestDeveloperLibrary;
     procedure TestDeveloperLibrary1;

  end;

implementation

procedure TTestSetting.TestInstantiation;
begin
  AssertTrue(fSetting <> nil);
end;

procedure TTestSetting.TestWriteKeepAliveTimeout;
begin
  fSetting.KeepAliveTimeout:= 1000;
  AssertEquals(fSetting.KeepAliveTimeout, 1000);
end;

procedure TTestSetting.TestBaudRate;
begin
  AssertEquals(fSetting.BaudRate, 19200);
end;

procedure TTestSetting.TestDeveloperLibrary;
begin
  writeln((TSetting.Create() as ISetting).DeveloperLibrary);
end;

procedure TTestSetting.TestDeveloperLibrary1;
var
  S: ISetting;
  L: string;
begin
  S := TSetting.Create();
  L := S.DeveloperLibrary;
  AssertTrue(S <> nil);
end;

procedure TTestSetting.SetUp;
begin
  fSetting := GetSetting;
end;


initialization

  RegisterTest(TTestSetting);
end.

