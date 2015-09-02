unit uTestAnsiLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, uLibrary, fpcunit, testutils, testregistry;

type

  { TTestAnsiLibrary }

  TTestAnsiLibrary= class(TTestCase)
  published
    procedure TestInstantiation;
    procedure TestReadShortDescription;
    procedure TestShortDescription;
  end;

implementation
var
  Lib: ILibrary;

procedure TTestAnsiLibrary.TestInstantiation;
begin
  AssertTrue(Lib <> nil);
end;

procedure TTestAnsiLibrary.TestReadShortDescription;
var
  Ml: IModule;
  M: IModuleDefine;
  Vs: IVars;
  V: IVarDefine;
  P: Pointer;
begin
  M := Lib[TTypeSublibrary.slDeveloper][13].ModuleDefine;
  for P  in Lib[TTypeSublibrary.slDeveloper] do
  begin
    Ml := Lib[TTypeSublibrary.slDeveloper].ExtractData(P);
    AssertTrue(Ml <> nil);
    Writeln(Ml.Name);
  end;

  AssertTrue(M <> nil);

  Vs := M.Registers[TTypeRegister.trInput];
  AssertTrue(Vs <> nil);

  for P in Vs do
  begin
    V := Vs.ExtractData(P);
    AssertTrue(V <> nil);
    Writeln(V.ShortDescription, V.Ver);
  end;

end;

procedure TTestAnsiLibrary.TestShortDescription;
var
  Ml: IModule;
  M: IModuleDefine;
  Vs: IVars;
  V: IVarDefine;
  P: Pointer;
begin
  M := Lib[TTypeSublibrary.slDeveloper][13].ModuleDefine;
  writeln(M.PreSets.BitsSet.BitsSet['test'].ShortDescription);

end;


initialization
  SetConsoleCP(1251);
  SetConsoleOutputCP(1251);
  RegisterTest(TTestAnsiLibrary);
  Lib := GetLibrary(['library\Developer\module.jlf', 'library\User\module.jlf']);

end.

