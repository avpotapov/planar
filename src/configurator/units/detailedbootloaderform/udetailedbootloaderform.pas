unit uDetailedBootloaderForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, Buttons, ExtCtrls, Windows,
  uConfiguratorData,
  uBootLoader,
  uLibrary;

type

  { TDetailedBootloaderForm }

  TDetailedBootloaderForm = class(TDetailedForm)
    BootLoad: TBitBtn;
    FileNameEdit: TFileNameEdit;
    FileNameLabel: TLabel;
    LogMemo: TMemo;
    Panel: TPanel;
    FileNamePanel: TPanel;
    ProgressBar: TProgressBar;
    procedure BootLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fModbusData:     TModbusData;
    fDeviceData:     TDeviceData;
    fBootloaderData: TBootloaderData;
    fBootloader:     IBootloader;

  private
    procedure ShowStatus(const aStatus: string);
    procedure ChangeProgress(const aSize, aCurrentPage: Word);
    procedure Terminate(Sender: TObject);
  public
    procedure Load(const aContentData: array of TContentData); override;
    procedure Unload; override;
  end;


implementation

resourcestring
  sFileFilter = 'Firmware file (*.bfm)|*.bfm|Firmware file (*.bin)|*.bin|All files (*.*)|*.*';
  sFormatTime = 'hh:nn:ss:zzz';
{$R *.lfm}

{ TDetailedBootloaderForm }

procedure TDetailedBootloaderForm.BootLoadClick(Sender: TObject);
begin
  try
    if not FileExists(Utf8ToAnsi(FileNameEdit.Text)) then
      raise Exception.Create('Не найден файл прошивки');
    if not fModbusData.Controller.IsOpen then
      raise Exception.Create('Включите канал связи');

    case fDeviceData.Module.TypeBootloader of
      bl1: fBootloader := TBootloader1.Create(
                            FileNameEdit.Text,
                            fDeviceData.SlaveId,
                            fModbusData.Controller,
                            @ChangeProgress,
                            @ShowStatus,
                            @Terminate);
      bl2: ;
      bl3: ;
    end;
    BootLoad.Enabled := False;
    fBootloader.Flash;



  except
    on E: Exception do
      MessageBox(Handle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure TDetailedBootloaderForm.FormCreate(Sender: TObject);
begin
  FileNameEdit.Filter := sFileFilter;
end;

procedure TDetailedBootloaderForm.ShowStatus(const aStatus: string);
begin
  LogMemo.Lines.Add(FormatDateTime(sFormatTime, Now) + ': ' + aStatus);
end;

procedure TDetailedBootloaderForm.ChangeProgress(const aSize, aCurrentPage: Word);
begin
  ProgressBar.Min      := 0;
  ProgressBar.Max      := aSize;
  ProgressBar.Position := aCurrentPage;
  if ProgressBar.Position = Integer(aSize) then
    ProgressBar.Position := 0;
end;

procedure TDetailedBootloaderForm.Terminate(Sender: TObject);
begin
  BootLoad.Enabled := True;
end;

procedure TDetailedBootloaderForm.Load(const aContentData: array of TContentData);
var
  I: Integer;
begin
  {$IFDEF DEBUG}
  Assert(Length(aContentData) = 3);
  {$ENDIF}

  // Инициализация
  for I := Low(aContentData) to High(aContentData) do
  begin
    if aContentData[I] is TModbusData then
      fModbusData := aContentData[I] as TModbusData;
    if aContentData[I] is TDeviceData then
      fDeviceData := aContentData[I] as TDeviceData;
    if aContentData[I] is TBootloaderData then
      fBootloaderData := aContentData[I] as TBootloaderData;
  end;

  BootLoad.Enabled := True;
end;

procedure TDetailedBootloaderForm.Unload;
var
  BoxStyle: Integer;

begin
  BoxStyle := MB_ICONWARNING + MB_OKCANCEL;
  Screen.Cursor := crHourGlass;
  try
    if ((fBootloader <> nil) and fBootloader.IsFlashing) then
      if  MessageBox(Handle, PChar(Utf8ToAnsi('Идет прошивка устройства'+ #13#10 +
                                              'Прервать прошивку - ''OK' + #13#10 +
                                              'Продолжить прошивку - ''CANCEL''')),
        PChar(Utf8ToAnsi('Предупреждение')), BoxStyle) = IDOK then
           fBootloader.StopFlash;

    while ((fBootloader <> nil) and fBootloader.IsFlashing) do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;
  finally
    fBootloader := nil;
    LogMemo.Lines.Clear;
    Screen.Cursor := crDefault;
  end;
end;

end.

