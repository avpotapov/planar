program configurator;

{$mode objfpc}{$H+}

uses
  ShareMem,
	{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, uholdings, virtualtreeview_package, umap, ulibrary,
  umodbus, usetting, uMainForm, uSettingForm, uSettingFormData, uDeviceForm,
  uSignature, uDetailedModbusForm, uDetailedDeviceForm, udetailedgroupform,
  uDetailedBackupForm, udetailedtrendsform, uDetailedBootloaderForm,
  uFrameListFactory, uGroupItemTreeNodeFactory, uConfiguratorData,
  uDetailedProxyForm, uAutoScan, uContentBuilder, uMainFormPopupMenu,
  uModbusFormData, uDeviceFormData, uBootLoader, uBackupFormData, uHoldingsData,
  uHoldingsForm, ucustomeditor, upropertyeditor, uSplashForm, uUpdateForm,
  uCheckUpdate, uUpdateInfoForm, uAboutForm, uAbout;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

