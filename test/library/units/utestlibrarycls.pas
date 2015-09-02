unit uTestLibraryCls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLibraryCls, uLibrary, fpcunit, testutils,
  testregistry;

type

  { TTestLibraryCls }

  TTestLibraryCls= class(TTestCase)
  private
    fModuleDefine: IModuleDefine;
  protected
    procedure SetUp; override;
  published
    procedure TestInstantiation;
    procedure TestPresets;
  end;

implementation


procedure TTestLibraryCls.SetUp;
begin
  fModuleDefine := TModuleDefine.Create;
end;

procedure TTestLibraryCls.TestInstantiation;
begin
  AssertTrue(fModuleDefine <> nil);
end;

procedure TTestLibraryCls.TestPresets;
var
  I: Integer;
  PickList: IPickList;
  PickItem: IPickItem;
begin
  I := fModuleDefine.PreSets.PickLists.Add('test');
  AssertTrue(I = 0);
  AssertTrue(fModuleDefine.PreSets.PickLists['test'] <> nil);
  PickList := fModuleDefine.PreSets.PickLists['test'];

  I := PickList.Add(10);
  AssertTrue(I = 0);
  AssertTrue(PickList[10] <> nil);
  PickItem := PickList[10];
  PickItem.Name := 'Test 10';

  I := PickList.Add(20);
  AssertTrue(I = 1);
  AssertTrue(PickList[20] <> nil);
  PickItem := PickList[20];
  PickItem.Name := 'Test 20';


  I := fModuleDefine.PreSets.PickLists.Add('test 2');
  AssertTrue(I = 1);
  AssertTrue(fModuleDefine.PreSets.PickLists['test 2'] <> nil);
  PickList := fModuleDefine.PreSets.PickLists['test 2'];

  I := PickList.Add(30);
  AssertTrue(I = 0);
  AssertTrue(PickList[30] <> nil);
  PickItem := PickList[30];
  PickItem.Name := 'Test 30';

  I := PickList.Add(40);
  AssertTrue(I = 1);
  AssertTrue(PickList[40] <> nil);
  PickItem := PickList[40];
  PickItem.Name := 'Test 40';



end;


initialization

  RegisterTest(TTestLibraryCls);
end.

