program testmodbus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umodbus, ubase, ucommunication, uconnection, ucontroller,
  ucontrollerpool, uframe, uframefactory, ulogger, userver, ustackbuilder,
  utransaction, utestform;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.

