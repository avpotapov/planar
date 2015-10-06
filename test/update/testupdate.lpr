program testupdate;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, utestupdate;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

