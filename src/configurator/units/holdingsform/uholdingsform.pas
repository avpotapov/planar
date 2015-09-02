unit uHoldingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, Buttons, Windows, uModbus, uLibrary, uHoldings,
  uConfiguratorData, uFrameListFactory, uHoldingsData;

type
  TTypeOperation  = (toSave, toRestore);
  THoldingsThread = class;
  { THoldingsForm }

  THoldingsForm = class(TForm)
    FileNameBox:  TGroupBox;
    FileNameEdit: TFileNameEdit;
    ImageList: TImageList;
    ProgressBar:  TProgressBar;
    StartButton:  TSpeedButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
  private
    fFrameListFactory: TFrameListFactory;
    fHoldingsWriterFrameListFactory: THoldingsWriterFrameListFactory;
    fFrameList:     TFrameList;
    fDeviceData:    TDeviceData;
    fModbusData:    TModbusData;
    fHoldings:      IHoldings;
    fHoldingsThread: THoldingsThread;
    fTypeOperation: TTypeOperation;
  private
    procedure CreateFrameList;
    procedure CreateWriteFrameList;
    procedure OnTerminate(Sender: TObject);
    procedure Stop(Sender: TObject);
  public
    procedure SaveHoldings(const aModbusData: TModbusData;
      const aDeviceData: TDeviceData; const aTypeOperation: TTypeOperation);
  end;


  { THoldingsThread }

  THoldingsThread = class(TThread)
  private
    fCurrentFrame: Integer;
    fClosed: Integer;
    fSavedHoldingsForm: THoldingsForm;
  private
    procedure DoUpdate;
  public
    constructor Create(const aSavedHoldingsForm: THoldingsForm); reintroduce;
  public
    function IsClosed: Boolean;
    procedure Close;
  end;

  { THoldingsRestorer }

  THoldingsRestorer = class(THoldingsThread)
  protected
    procedure Execute; override;
  end;

  { THoldingsSaver }

  THoldingsSaver = class(THoldingsThread)
  private
    procedure DoCreateHoldings;
  protected
    procedure Execute; override;
  end;


implementation

{$R *.lfm}

resourcestring
  sFilter = 'Holding register files|*.hrf|All files|*.*';


{ THoldingsForm }

procedure THoldingsForm.FormCreate(Sender: TObject);
begin
  fFrameListFactory := TFrameListFactory.Create;
  fHoldingsWriterFrameListFactory := THoldingsWriterFrameListFactory.Create;
  fFrameList := TFrameList.Create;
  FileNameEdit.Filter := sFilter;
  FileNameEdit.InitialDir := ExtractFilePath(ParamStr(0));

end;

procedure THoldingsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Screen.Cursor := crHourGlass;
  try
    if (fHoldingsThread <> nil) then
    begin
      while not fHoldingsThread.IsClosed do
      begin
        Application.ProcessMessages;
        Sleep(10);
        CanClose := False;
      end;
      fHoldingsThread.Free;
      fHoldingsThread := nil;
    end;


  finally
    Screen.Cursor := crDefault;
    CanClose := True;
  end;
end;

procedure THoldingsForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fHoldingsWriterFrameListFactory);
  FreeAndNil(fFrameListFactory);
  FreeAndNil(fFrameList);
end;

procedure THoldingsForm.StartButtonClick(Sender: TObject);
begin
  if fHoldingsThread <> nil then
  begin
    while not fHoldingsThread.IsClosed do
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
    fHoldingsThread.Free;
    fHoldingsThread := nil;
  end;


  try
    case fTypeOperation of
      toSave:
      begin
        CreateFrameList;

        fHoldingsThread := THoldingsSaver.Create(Self);
      end;
      toRestore:
      begin
        fHoldings := GetHoldings(FileNameEdit.FileName);
        CreateWriteFrameList;
        fHoldingsThread := THoldingsRestorer.Create(Self);
      end;
    end;

    ImageList.GetBitmap(1, StartButton.Glyph);
    StartButton.Hint := 'Остановить';
    fHoldingsThread.OnTerminate := @OnTerminate;
    fHoldingsThread.Start;
    StartButton.OnClick := @Stop;


  except
    on E: Exception do
      MessageBox(Handle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

procedure THoldingsForm.OnTerminate(Sender: TObject);
begin
  ImageList.GetBitmap(0, StartButton.Glyph);
  StartButton.Hint := 'Запустить';
  StartButton.OnClick := @StartButtonClick;
end;

procedure THoldingsForm.Stop(Sender: TObject);
begin
  fHoldingsThread.Close;
end;


procedure THoldingsForm.CreateFrameList;
var
  Vars: IVars;
  VarDefine: IVarDefine;
  P: Pointer;
  FoundIndex: Integer;
begin
  if fDeviceData.Module = nil then
    raise Exception.Create('Отсутствует описание устройства');

  FoundIndex := fDeviceData.Module.ModuleDefine.Registers.IndexOf(TTypeRegister.trHolding);
  if FoundIndex < 0 then
    raise Exception.Create(Format('В модуле ''%s'' отсутствует описание Holding-регистров', [fDeviceData.Module.Name]));

  Vars := fDeviceData.Module.ModuleDefine.Registers.Data[FoundIndex];
  for P in Vars do
  begin
    VarDefine := Vars.ExtractData(P);
    fFrameListFactory.VarList.Add(VarDefine);
  end;

  fFrameListFactory.CreateFrameList(fDeviceData.SlaveId, fFrameList);
end;

procedure THoldingsForm.CreateWriteFrameList;
begin
  fHoldingsWriterFrameListFactory.Holdings := fHoldings;
  fHoldingsWriterFrameListFactory.CreateFrameList(fDeviceData.SlaveId, fFrameList);
end;

procedure THoldingsForm.SaveHoldings(const aModbusData: TModbusData;
  const aDeviceData: TDeviceData; const aTypeOperation: TTypeOperation);
begin
  fModbusData    := aModbusData;
  fDeviceData    := aDeviceData;
  fTypeOperation := aTypeOperation;

  case aTypeOperation of
    toSave: Caption    := 'Сохранить Holding-регистры в файл';
    toRestore: Caption := 'Восстановить Holding-регистры';
  end;
  ShowModal;
end;


{ THoldingsThread }

constructor THoldingsThread.Create(const aSavedHoldingsForm: THoldingsForm);
begin
  inherited Create(False);
  fSavedHoldingsForm := aSavedHoldingsForm;
  fClosed := 1;
  fCurrentFrame := 0;
  FreeOnTerminate := False;
end;

procedure THoldingsThread.DoUpdate;
begin
  with fSavedHoldingsForm do
  begin
    ProgressBar.Min      := 0;
    ProgressBar.Max      := fFrameList.Count - 1;
    ProgressBar.Position := fCurrentFrame;
    if ProgressBar.Position = Integer(fFrameList.Count - 1) then
      ProgressBar.Position := 0;
  end;
end;


function THoldingsThread.IsClosed: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fClosed, 0, 0) = 1;
end;

procedure THoldingsThread.Close;
begin
  Windows.InterLockedExchange(fClosed, 1);
  WaitForSingleObject(Handle, 1000);
end;

{ THoldingsRestorer }
const
  WM_COUNT = WM_USER + 1;

procedure THoldingsRestorer.Execute;
var
  Frame: IFrame;
begin
  Windows.InterlockedExchange(fClosed, 0);
  try

    for Frame in fSavedHoldingsForm.fFrameList do
    begin

      // Пока не отправлен запрос
      while not fSavedHoldingsForm.fModbusData.Controller.InQueue(Frame) do
      begin
        Sleep(20);
        if IsClosed then
          Exit;
      end;

      // Обработка ответа
      if Frame.Responded then
      begin
        PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
        Inc(fCurrentFrame);
        Synchronize(@DoUpdate);
      end
      else
        PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);

      if IsClosed then
          Exit;
    end;
    PostMessage(fSavedHoldingsForm.Handle, WM_CLOSE, 0, 0);
  finally
    Windows.InterlockedExchange(fClosed, 1);
  end;
end;


procedure THoldingsSaver.DoCreateHoldings;
var
  Holdings:  IHoldings;
  VarDefine: IVarDefine;
begin
  Holdings     := CreateHoldings;

  Holdings.Created := Now;
  Holdings.ModuleName := fSavedHoldingsForm.fDeviceData.Module.Name;
  Holdings.Uid := fSavedHoldingsForm.fDeviceData.Module.Uid;

  for VarDefine in fSavedHoldingsForm.fFrameListFactory.VarList do
    THoldingBuilder.BuildHolding(Holdings.AddNew, VarDefine, fSavedHoldingsForm.fDeviceData.Map);

  SaveHoldings(Holdings, fSavedHoldingsForm.FileNameEdit.FileName);
end;

procedure THoldingsSaver.Execute;
var
  Frame: IFrame;
begin
  Windows.InterlockedExchange(fClosed, 0);
  try

    for Frame in fSavedHoldingsForm.fFrameList do
    begin

      // Пока не отправлен запрос
      while not fSavedHoldingsForm.fModbusData.Controller.InQueue(Frame) do
      begin
        Sleep(20);
        if IsClosed then
          Exit;
      end;

      // Обработка ответа
      if Frame.Responded then
      begin
        fSavedHoldingsForm.fDeviceData.Map.WriteData(Swap(PWord(@Frame.RequestPdu^[1])^),
          Frame.ResponsePdu);
        PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
        Inc(fCurrentFrame);
        Synchronize(@DoUpdate);
      end
      else
        PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);

      if IsClosed then
          Exit;
    end;
    Synchronize(@DoCreateHoldings);
    PostMessage(fSavedHoldingsForm.Handle, WM_CLOSE, 0, 0);
  finally
    Windows.InterlockedExchange(fClosed, 1);
  end;

end;

end.
