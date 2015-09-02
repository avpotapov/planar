program writeread;

{$mode objfpc}{$H+}

uses
  ShareMem, Interfaces, Forms, umap, umodbus, GuiTestRunner, uTestWriteRead;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

