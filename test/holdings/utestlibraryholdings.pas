unit uTestLibraryHoldings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uHoldings, uLibrary, fpcunit, testutils, testregistry;

type

  { TTestLibraryToHolding }

  TTestLibraryToHolding= class(TTestCase)
  published
    procedure TestLibraryToHolding;
  end;

implementation

procedure TTestLibraryToHolding.TestLibraryToHolding;
var
  L: ILibrary;
  Vs: IVars;
  Hs: IHoldings;
  H: IHolding;
  P: Pointer;
  V: IVarDefine;
begin
  L := GetLibrary(['library\Developer\module.jlf', 'library\User\module.jlf']);
  AssertTrue(L <> nil);
  Vs := L[TTypeSublibrary.slDeveloper][13].ModuleDefine.Registers[TTypeRegister.trHolding];
  AssertTrue(Vs <> nil);
  Hs := CreateHoldings;
  AssertTrue(Hs <> nil);
  for P in Vs do
  begin
    V := Vs.ExtractData(P);
    Hs.AddNew.ShortDescription := V.ShortDescription;
    Writeln(V.ShortDescription);
  end;
  SaveHoldings(Hs, 'saved\test.hrf');



end;



initialization

  RegisterTest(TTestLibraryToHolding);
end.

