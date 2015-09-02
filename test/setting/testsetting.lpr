program testsetting;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, usetting, GuiTestRunner, utestsetting;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

