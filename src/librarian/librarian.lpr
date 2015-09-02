program librarian;

{$mode objfpc}{$H+}

uses
  ShareMem,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ucustomeditor, usettingform, usettingformdata, usplashform, usetting,
  ulibrary, virtualtreeview_package, uMainForm,
  uEditingFormPool, uModuleForm, uEmptyForm,
  ubasedescform, uPickListForm, uvarsform, uConfigForm,
  uPickItemEditor, uBitsForm, uBitEditor, uVarEditor,
 uPickListEditor, uBitsEditor, uMenuHelper,
uAdvancedPropertyEditor, u2ColumnEditor, uVarsEditor, uLibraryEditor, 
uLibraryData, uLibraryBuilder, uregisterseditor, uConfigEditor;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

