program testmap;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, umap, GuiTestRunner, utestmap;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

