unit utestupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uCheckUpdate;

type

  { TTestUpdate }

  TTestUpdate= class(TTestCase)
  private
    FUpdate: TUpdate;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParse;
    procedure TestGetFile;
  end;

implementation

procedure TTestUpdate.TestParse;
begin
	FUpdate.Parse;
  AssertNotNull(FUpdate.UpdateLib);
  AssertNotNull(FUpdate.UpdateSoft);
  CheckEquals(FUpdate.UpdateLib.Count, 2);
  CheckEquals(FUpdate.UpdateSoft.Count, 2);
end;

procedure TTestUpdate.TestGetFile;
begin
  CheckTrue(FUpdate.GetUpdateFile);
end;

procedure TTestUpdate.SetUp;
begin
  FUpdate := TUpdate.Create;
end;

procedure TTestUpdate.TearDown;
begin
 FUpdate.Free;
end;

initialization

  RegisterTest(TTestUpdate);
end.

