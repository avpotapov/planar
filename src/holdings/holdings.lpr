library holdings;

{$mode objfpc}{$H+}

uses
  Sharemem, Classes, uholdings, uHoldingsCls, ulibrary, uSaxBase,
uHoldingsBuilder, uHoldingsSaxParser, uHoldingsSerialize, uString;

 function CreateHoldings: IHoldings; export;
 begin
   Result := THoldings.Create as IHoldings;
 end;


  function GetHoldings(const aFileName: string): IHoldings; export;
  var
    Factory: THoldingsSaxFactory;
    Holdings: IHoldings;
  begin
    Factory := THoldingsSaxFactory.Create;
    try
      Holdings := Factory.GetHoldings(aFileName);
    finally
      Factory.Free;
    end;
    Result := Holdings;
  end;

procedure SaveHoldings(const aHoldings: IHoldings; const aFileName: string); export;
begin
  THoldingsDomFactory.Serialize(aHoldings, aFileName);
end;


exports
  CreateHoldings,
  GetHoldings,
  SaveHoldings;

{$R *.res}

begin
end.

