unit uTestHoldings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System, uholdings, fpcunit, testutils, testregistry, Windows;

type

  { TTestHoldings }

  TTestHoldings= class(TTestCase)
  published
    procedure TestInstantiation;
    procedure TestAdd;
    procedure TestEnumerator;
    procedure TestRemove;
  end;

implementation




{ TTestHoldings }

procedure TTestHoldings.TestInstantiation;
var
  Holdings: IHoldings;
begin
  Holdings := CreateHoldings;
  AssertTrue(Holdings <> nil);
end;

procedure TTestHoldings.TestAdd;
var
  Holdings: IHoldings;
  H: IHolding;
begin
  Holdings := CreateHoldings;
  AssertTrue(Holdings <> nil);
  AssertTrue(Holdings.Count = 0);
  H := Holdings.AddNew;
  AssertTrue(Holdings.Count = 1);
  H := Holdings.AddNew;
  AssertTrue(Holdings.Count = 2);
  H := Holdings.AddNew;
  AssertTrue(Holdings.Count = 3);
//  InterLockedDecrement();
//  HandleError(204);
end;

procedure TTestHoldings.TestEnumerator;
var
  Holdings: IHoldings;
  H: IHolding;
  S: TInterfacedObject;
begin
  Holdings := CreateHoldings;
  AssertTrue(Holdings <> nil);
  AssertTrue(Holdings.Count = 0);

  H := Holdings.AddNew;
  H.Name:= AnsiToUtf8('Holding 1');
  H.Value := 1;
  H.ValueStr:= 'Test1';
  AssertTrue(Holdings.Count = 1);

  H := Holdings.AddNew;
  H.Name:= AnsiToUtf8('Holding 2');
  H.Value := 2;
  H.ValueStr:= 'Test2';
  AssertTrue(Holdings.Count = 2);

  H := Holdings.AddNew;
  H.Name:= AnsiToUtf8('Holding 3');
  H.Value := 3;
  H.ValueStr:= 'Test3';
  AssertTrue(Holdings.Count = 3);

  for H in Holdings do
    Writeln(H.Value);



end;

procedure TTestHoldings.TestRemove;
var
  Holdings: IHoldings;
  H: IHolding;

begin
  Holdings := CreateHoldings;
  AssertTrue(Holdings <> nil);
  AssertTrue(Holdings.Count = 0);

  H := Holdings.AddNew;
  H.Name:= AnsiToUtf8('Holding 1');
  H.Value := 1;
  AssertTrue(Holdings.Count = 1);

  H := Holdings.AddNew;
  H.Name:= AnsiToUtf8('Holding 2');
  H.Value := 2;
  AssertTrue(Holdings.Count = 2);

  H := Holdings.AddNew;
  H.Name:= AnsiToUtf8('Holding 3');
  H.Value := 3;
  AssertTrue(Holdings.Count = 3);

  for H in Holdings do
    Writeln(H.Value);

  Holdings.Remove(Holdings.Last);
  AssertTrue(Holdings.Count = 2);

  Holdings.Remove(Holdings[1]);
  AssertTrue(Holdings.Count = 1);

  Holdings.Remove(Holdings.First);
  AssertTrue(Holdings.Count = 0);

  for H in Holdings do
    Writeln(H.Value);

end;

initialization

  RegisterTest(TTestHoldings);
end.

