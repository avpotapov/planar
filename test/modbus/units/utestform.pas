unit uTestForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Windows, fgl,
  umodbus, ustackbuilder;

const
  WM_COUNT_RTU = WM_USER + 1;
  RTU_COUNT    = 10;

  WM_COUNT_TCP = WM_USER + 2;
  TCP_COUNT    = 10;

type
  TFrames = specialize TFpgList<IFrame>;

  { TTestForm }

  TTestForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    TcpBox:   TGroupBox;
    RtuCount: TLabel;
    TcpCount: TLabel;
    TcpStartButton: TButton;
    RtuStopButton: TButton;
    RtuStartButton: TButton;
    RtuBox:   TGroupBox;
    TcpStopButton: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure RtuStartButtonClick(Sender: TObject);
    procedure RtuStopButtonClick(Sender: TObject);
    procedure TcpStartButtonClick(Sender: TObject);
    procedure TcpStopButtonClick(Sender: TObject);
  private
    // Счетчик
    fTcpSuccess, fTcpBad: Longint;
    fTcpActive:     Integer;
    fTcpController: IController;
    fTcpFrames:     TFrames;
    TcpThreadIds:   array[0 .. TCP_COUNT - 1] of TThreadId;

    fRtuSuccess, fRtuBad: Longint;
    fRtuActive:     Integer;
    fRtuController: IController;
    fRtuFrames:     TFrames;
    RtuThreadIds:   array[0 .. RTU_COUNT - 1] of TThreadId;

  private
    function GetTcpActive: Boolean;
    procedure SetTcpActive(const aActive: Boolean);
    procedure WmCountTcp(var Message: TMessage); message WM_COUNT_TCP;

    function GetRtuActive: Boolean;
    procedure SetRtuActive(const aActive: Boolean);
    procedure WmCountRtu(var Message: TMessage); message WM_COUNT_RTU;
  public
    property RtuActive: Boolean read GetRtuActive write SetRtuActive;
    property TcpActive: Boolean read GetTcpActive write SetTcpActive;
  end;

var
  TestForm: TTestForm;

implementation

//uses ucommunication, uconnection, ucontroller, ustackbuilder, utransaction;

{$R *.lfm}


function PollRtu(aParameter: Pointer): Integer;
var
  Frame: IFrame;
  Form:  TTestForm;
begin
  Result := 0;
  Form   := TTestForm(aParameter);
  Form.RtuActive := True;
  while Form.RtuActive do
  begin
    for Frame in Form.fRtuFrames do
    begin
      while not Form.fRtuController.InQueue(Frame) do
      begin
        sleep(20);
        if not Form.RtuActive then
          Exit;
      end;

      if Frame.Responded then
        PostMessage(Form.Handle, WM_COUNT_RTU, 1, 0)
      else
        PostMessage(Form.Handle, WM_COUNT_RTU, 0, 1);

      if not Form.RtuActive then
        Exit;
    end;
  end;
end;


function PollTcp(aParameter: Pointer): Integer;
var
  Frame: IFrame;
  Form:  TTestForm;
begin
  Result := 0;
  Form   := TTestForm(aParameter);
  Form.TCpActive := True;
  while Form.TCpActive do
  begin
    for Frame in Form.fTcpFrames do
    begin
      while not Form.fTcpController.InQueue(Frame) do
      begin
        sleep(20);
        if not Form.TcpActive then
          Exit;
      end;

      if Frame.Responded then
        PostMessage(Form.Handle, WM_COUNT_TCP, 1, 0)
      else
        PostMessage(Form.Handle, WM_COUNT_TCP, 0, 1);

      if not Form.TcpActive then
        Exit;
    end;
  end;
end;

{ TTestForm }

procedure TTestForm.RtuStartButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(fRtuController) and fRtuController.IsOpen then
    Exit;

  fRtuController := GetRtuBuilder.GetController('COM3', 19200, 2, 0, 8, 30);
  fRtuController.Open;

  for I := 0 to RTU_COUNT - 1 do
    RtuThreadIds[I] := BeginThread(@PollRtu, Self);
end;

procedure TTestForm.FormCreate(Sender: TObject);
begin
  fRtuFrames := TFrames.Create;
  // Добавить тестовые запросы
  fRtuFrames.Add(ReadInput(1, 1, 2, 1000));
  fRtuFrames.Add(ReadInput(1, 1, 20, 3000));
  fRtuFrames.Add(ReadInput(1, 5, 20, 3000));

  //  fRtuFrames.Add(ReadInput(15, 1, 10, 3000));


  fTcpFrames := TFrames.Create;
  // Добавить тестовые запросы
  fTcpFrames.Add(ReadInput(1, 1, 2, 1000));
  fTcpFrames.Add(ReadHolding(1, 11, 2, 3000));

  fTcpController := nil;
end;

procedure TTestForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  RtuStopButtonClick(Sender);
  TcpStopButtonClick(Sender);
end;

procedure TTestForm.Button1Click(Sender: TObject);
var
//  RtuBuilder: IRtuBuilder;
  RtuController: IController;
begin
  RtuController := GetRtuBuilder.GetController('COM3', 19200, 2, 0, 8, 30);
  Label1.Caption := RtuController.ToString;
end;

procedure TTestForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fRtuFrames);
  FreeAndNil(fTcpFrames);
end;

procedure TTestForm.Label2Click(Sender: TObject);
begin

end;

procedure TTestForm.RtuStopButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(fRtuController) then
  begin
    if not fRtuController.IsOpen then
      Exit;

    Screen.Cursor := crHourGlass;
    try


      RtuActive := False;
      Sleep(100);

      for I := 0 to RTU_COUNT - 1 do
      begin
        if RtuThreadIds[I] <> 0 then
        begin
          WaitForThreadTerminate(RtuThreadIds[I], 1000);
          CloseThread(RtuThreadIds[I]);
          RtuThreadIds[I] := 0;

          Application.ProcessMessages;
          Sleep(100);

        end;
      end;

      fRtuController.Close;

    finally
      Screen.Cursor := crDefault;
    end;

  end;
end;

procedure TTestForm.TcpStartButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(fTcpController) and fTcpController.IsOpen then
    Exit;

  fTcpController := GetTcpBuilder.GetController('192.168.0.166', 502);
  fTcpController.Open;

  for I := 0 to TCP_COUNT - 1 do
    TcpThreadIds[I] := BeginThread(@PollTcp, Self);

end;

procedure TTestForm.TcpStopButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(fTcpController) then
  begin
    if not fTcpController.IsOpen then
      Exit;

    Screen.Cursor := crHourGlass;
    try


      TcpActive := False;
      Sleep(100);

      for I := 0 to TCP_COUNT - 1 do
      begin
        if TcpThreadIds[I] <> 0 then
        begin
          WaitForThreadTerminate(TcpThreadIds[I], 1000);
          CloseThread(TcpThreadIds[I]);
          TcpThreadIds[I] := 0;

          Application.ProcessMessages;
          Sleep(100);

        end;
      end;

      fTcpController.Close;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

function TTestForm.GetTcpActive: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fTcpActive, 0, 0) = 1;
end;

procedure TTestForm.SetTcpActive(const aActive: Boolean);
begin
  case aActive of
    True: Windows.InterLockedExchange(fTcpActive, 1);
    False: Windows.InterLockedExchange(fTcpActive, 0);
  end;
end;

procedure TTestForm.WmCountTcp(var Message: TMessage);
begin
  if Message.WParam = 1 then
    Inc(fTcpSuccess);
  if Message.LParam = 1 then
    Inc(fTcpBad);
  TcpCount.Caption := Format('%d : %d', [fTcpSuccess, fTcpBad]);
end;

function TTestForm.GetRtuActive: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fRtuActive, 0, 0) = 1;
end;

procedure TTestForm.SetRtuActive(const aActive: Boolean);
begin
  case aActive of
    True: Windows.InterLockedExchange(fRtuActive, 1);
    False: Windows.InterLockedExchange(fRtuActive, 0);
  end;
end;

procedure TTestForm.WmCountRtu(var Message: TMessage);
begin
  if Message.WParam = 1 then
    Inc(fRtuSuccess);
  if Message.LParam = 1 then
    Inc(fRtuBad);
  RtuCount.Caption := Format('%d : %d', [fRtuSuccess, fRtuBad]);
end;

end.
