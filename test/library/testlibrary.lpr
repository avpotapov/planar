program testlibrary;

{$mode objfpc}{$H+}

uses
  ShareMem, Interfaces, Forms, ulibrary, ubase, ulibrarycls, usaxbase, ulibrarybuilder,
  ulibrarysaxparser, umodulebuilder, GuiTestRunner, uTestLibrary,
  uTestSaxParser, uTestLibraryCls, uTestAnsiLibrary, uTestSerializer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

