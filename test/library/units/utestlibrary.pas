unit uTestLibrary;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uLibrary, fpcunit, testutils,
  testregistry;

type

  { TTestLibrary }

  TTestLibrary= class(TTestCase)
  private
    fLibrary: ILibrary;
  protected
    procedure SetUp; override;
  published
    procedure TestInstantiation;
  end;

implementation


procedure TTestLibrary.SetUp;
begin
  fLibrary := GetLibrary(['library\Developer\module.jlf', 'library\User\module.jlf']);
  Writeln(fLibrary.SubLibrary[TTypeSublibrary.slDeveloper].Module[13].Name);
end;

procedure TTestLibrary.TestInstantiation;
begin
  AssertTrue(fLibrary <> nil);
end;


initialization

  RegisterTest(TTestLibrary);
end.

