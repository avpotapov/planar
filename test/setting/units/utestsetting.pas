unit utestsetting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usetting, fpcunit, testregistry;

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
  writeln(fSetting.DeveloperLibrary);
end;

procedure TTestSetting.SetUp;
begin
  fSetting := GetSetting;
end;


initialization

  RegisterTest(TTestSetting);
end.

