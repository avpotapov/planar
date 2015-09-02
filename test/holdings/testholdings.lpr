program testholdings;

{$mode objfpc}{$H+}

uses
  ShareMem, Interfaces, Forms, ulibrary, uholdings, uholdingsbuilder,
  uholdingscls, uholdingssaxparser, usaxbase, uholdingsserialize, GuiTestRunner,
  uTestHoldings, uTestParserHoldings, uTestLibraryHoldings;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

