unit uTestSaxParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uLibrary, ulibrarybuilder, ulibrarysaxparser;

type

  { TTestSaxParser }

  TTestSaxParser= class(TTestCase)
  private
    fFactory: TLibraryFactory;
    fLibrary: ILibrary;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInstantiation;
    procedure TestGetLibrary;
    procedure TestPickList;
    procedure TestMeasure;
  end;

implementation

procedure TTestSaxParser.SetUp;
begin
  fFactory := TLibraryFactory.Create;
end;

procedure TTestSaxParser.TearDown;
begin
  fFactory.Free;
end;

procedure TTestSaxParser.TestInstantiation;
begin
  AssertTrue(fFactory <> nil);
end;

procedure TTestSaxParser.TestGetLibrary;
begin
  //fLibrary := fFactory.GetLibrary(['library\Developer\module.jlf', 'library\User\module.jlf']);
  //AssertTrue(fLibrary <> nil);
  //AssertEquals(fLibrary.Count, 2);
  //AssertTrue(fLibrary[TTypeSublibrary.slDeveloper] <> nil);
  //AssertTrue(fLibrary[TTypeSublibrary.slUser] <> nil);
end;

procedure TTestSaxParser.TestPickList;
var
  M: IModuleDefine;
  P: Pointer;
  V: IVarDefine;
begin
  fLibrary := fFactory.GetLibrary(['library\Developer\module.jlf', 'library\User\module.jlf']);
  AssertTrue(fLibrary[TTypeSublibrary.slDeveloper] <> nil);
  M := fLibrary[TTypeSublibrary.slDeveloper][13].ModuleDefine;
  AssertTrue(M <> nil);
  Writeln('----------- PickList From Presets --------------');
  for P in  M.PreSets.PickLists['test'] do
  begin
    Writeln(M.PreSets.PickLists['test'].ExtractKey(P));
    Writeln(M.PreSets.PickLists['test'].ExtractData(P).Name);
  end;

  Writeln('----------- PickList From VarDefine --------------');
  V := M.Registers[TTypeRegister.trInput]['AB696DC2-CF2B-498E-9B0C-3E3F83784D9B'];
  AssertTrue(V <> nil);

  for P in  V.PickList do
  begin
    Writeln(V.PickList.ExtractKey(P));
    Writeln(V.PickList.ExtractData(P).Name);
  end;

  Writeln('----------- PickList From VarDefine --------------');
  V := M.Registers[TTypeRegister.trInput]['852EF5BC-4EE0-420A-9F01-F19952AEE21B'];
  AssertTrue(V <> nil);

  for P in  V.PickList do
  begin
    Writeln(V.PickList.ExtractKey(P));
    Writeln(V.PickList.ExtractData(P).Name);
  end;

  Writeln(M.Configuration.GroupsList.Count);
end;

procedure TTestSaxParser.TestMeasure;
var
  M: IModuleDefine;
  P: Pointer;
  V: IVarDefine;
  Vs: IVars;
  I: Integer;
begin
  I := 0;
  fLibrary := fFactory.GetLibrary(['library\Developer\module.jlf', 'library\User\module.jlf']);
  AssertTrue(fLibrary[TTypeSublibrary.slDeveloper] <> nil);
  M := fLibrary[TTypeSublibrary.slDeveloper][13].ModuleDefine;
  Vs := M.Registers[TTypeRegister.trInput];
  for P in Vs do
  begin
    V := Vs.ExtractData(P);
    if Length(V.Measure) > 0 then
    begin
      writeln(I, V.Measure);
      Inc(I);
    end;
  end;


end;

initialization

  RegisterTest(TTestSaxParser);
end.

