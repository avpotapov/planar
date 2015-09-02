unit uTestParserHoldings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uHoldings, uholdingssaxparser, uholdingsserialize, fpcunit,
  testutils, testregistry;

type

  { TTestParserHoldings }

  TTestParserHoldings= class(TTestCase)
  published
    procedure TestSaxParser;
    procedure TestDomParser;
  end;

implementation

{ TTestParserHoldings }

procedure TTestParserHoldings.TestSaxParser;
var
  Holdings: IHoldings;
begin
  Holdings := GetHoldings('saved\holdings.xml');
  AssertTrue(Holdings.Count = 3);
  WriteLn(Holdings.First.Name);


end;

procedure TTestParserHoldings.TestDomParser;
var
  Holdings: IHoldings;
begin
  Holdings := GetHoldings('saved\holdings.xml');
  SaveHoldings(Holdings, 'saved\holdings1.xml');
end;

initialization

  RegisterTest(TTestParserHoldings);
end.

