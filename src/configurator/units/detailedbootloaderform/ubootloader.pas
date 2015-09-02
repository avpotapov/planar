unit uBootLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Windows, Forms,
  uModbus;

type
  TWriteEvent = procedure(const aSize, aCurrentPage: Word) of object;

  { IBootloader }

  IBootloader = interface
    ['{EBCE0859-39E7-453D-AA36-A5F12A53D7D7}']
    procedure Flash;
    procedure StopFlash;
    function IsFlashing: Boolean;
  end;

  { TBootloader }

  TPages = specialize TFpgList<IFrame>;
  TThreadFlash = class;

  TBootloader = class(TInterfacedObject, IBootloader)
  protected
    // Адрес устройства
    fSlaveId:    Byte;
    // Канал связи
    fController: IController;
    // Таймаут ответа устройства
    fTimeout:    Dword;
    // Имя файла и файловый поток
    fFileName:   String;
    fStream:     TFileStream;
    // Контейнер страниц из файлового потока
    fPages:      TPages;
    // Поток прошивки
    fThreadFlash: TThreadFlash;

    // Обработчики событий
    fOnWrite:  TWriteEvent;
    fOnStatus: TGetStrProc;
    fOnTerminate: TNotifyEvent;
  protected
    procedure CreatePages; virtual; abstract;
    function GetPages: TPages;

  public
    constructor Create(
      const aFileName: String;
      const aSlaveId: Byte;
      const aController: IController;
      const aOnWrite: TWriteEvent = nil;
      const aOnStatus: TGetStrProc = nil;
      const aOnTerminate: TNotifyEvent = nil); reintroduce;
    procedure AfterConstruction; override;
    destructor Destroy; override;

  public
    procedure Flash; virtual; abstract;
    procedure StopFlash;
    function IsFlashing: Boolean;
  end;

  { TBootloader1 }

  TBootloader1 = class(TBootloader)
  private
    fRunBootLoader:  IFrame;
    fRunApplication: IFrame;
  protected
    procedure CreatePages; override;
    procedure CreateCommands;
  public
    procedure Flash; override;
  end;

  { TThreadFlash }

  TThreadFlash = class(TThread)
  protected
    fFlashing:   Integer;
    fBootloader: TBootLoader;
    fStatus:     String;
    fSize, fCurrentPage: Word;
  protected
    procedure DoWrite;
    procedure DoStatus;
  public
    constructor Create(const aBootloader: TBootloader); reintroduce;
  public
    function IsFlashing: Boolean;
    procedure Stop;
  end;

  { TThreadFlash1 }

  TThreadFlash1 = class(TThreadFlash)
  private
    function RunBootloader(const aFrame: IFrame): Boolean;
    function RunApplication(const aFrame: IFrame): Boolean;
    function WritePage(const aFrame: IFrame): Boolean;
  protected
    procedure Execute; override;
  end;

implementation
uses
//  Logger,
  uSetting;

const
  WM_COUNT = WM_USER + 1;

{ TBootloader }

function TBootloader.GetPages: TPages;
begin
  Result := fPages;
end;

constructor TBootloader.Create(const aFileName: String; const aSlaveId: Byte;
  const aController: IController; const aOnWrite: TWriteEvent;
  const aOnStatus: TGetStrProc; const aOnTerminate: TNotifyEvent);
begin
  inherited Create;

  fFileName   := aFileName;
  fSlaveId    := aSlaveId;
  fController := aController;
  fPages      := TPages.Create;
  fThreadFlash := nil;
  fOnWrite    := aOnWrite;
  fOnStatus   := aOnStatus;
  fOnTerminate := aOnTerminate;
end;

procedure TBootloader.AfterConstruction;
begin
  inherited AfterConstruction;

  fTimeout := GetSetting.Timeout;

  fStream := TFileStream.Create(Utf8ToAnsi(fFileName), fmOpenRead);
  fStream.Position := 0;

end;

destructor TBootloader.Destroy;
begin
  StopFlash;
  FreeAndNil(fPages);
  FreeAndNil(fStream);
  inherited Destroy;
end;

procedure TBootloader.StopFlash;
begin
  if fThreadFlash <> nil then
  begin
    fThreadFlash.Stop;
    fThreadFlash.Free;
    fThreadFlash := nil;
  end;
end;

function TBootloader.IsFlashing: Boolean;
begin
  Result := (fThreadFlash <> nil) and fThreadFlash.IsFlashing;
end;


{ TBootloader1 }

procedure TBootloader1.CreatePages;
const
  NBB_BLOCK: Word = 990;    // Последняя страница во Flash
  MAX_FILE_SIZE   = 126720; // Максимальный размер файла

  SizeOfPage = 128;
var

  // Текущая страница
  CurrentPage: Word;
  CRC16: Word;
  // Количество байт для записи
  NumberBytesToWrite: Byte;

  // Размер буфера данных
  Buffer: TBuffer;

  I: Integer;

  // Фрейм запроса
  Frame: IFrame;
begin
  if fStream = nil then
    Exit;

  CRC16 := $FFFF;
  CurrentPage := 0;


  // Очищаем буфер
  FillChar({%H-}Buffer, SizeOf(Buffer), $0);


  // Проверка файла
  if (fStream.Size = 0) or (fStream.Size > MAX_FILE_SIZE) then
    raise Exception.Create(Format('Недопустимый размер файла ''%s''', [fFileName]));

  // 1. Пишем содержимое прошивки из файла
  repeat
    // Читаем очередные данные
    NumberBytesToWrite := fStream.Read(Buffer, SizeOfPage);
    if NumberBytesToWrite = 0 then
      Break;

    // Если количество прочитанных байт меньше размера страницы, оставшиеся данные заполняем 1
    if NumberBytesToWrite < SizeOfPage then
      for I := NumberBytesToWrite to SizeOfPage - 1 do
        Buffer[I] := $FF;

    // Код CRC16
    CalcCRC16(@Buffer, SizeOfPage, CRC16);

    // Создаем фрейм запроса и помещаем его в контейнер
    Frame := WritePage(fSlaveId, CurrentPage, SizeOfPage, @Buffer, fTimeout);
    fPages.Add(Frame);

    Inc(CurrentPage);
  until CurrentPage = 0;

  // 2. Стираем оставшиеся страницы
  // Очищаем буфер
  FillChar(Buffer, SizeOf(Buffer), $FF);
  while CurrentPage < NBB_BLOCK do
  begin
    // Код CRC16
    CalcCRC16(@Buffer, SizeOfPage, CRC16);

    // Создаем фрейм запроса и помещаем его в контейнер
    Frame := WritePage(fSlaveId, CurrentPage, SizeOfPage, @Buffer, fTimeout);
    fPages.Add(Frame);

    Inc(CurrentPage);
  end;

  // 3. Запись контрольной суммы
  Buffer[0] := Lo(CRC16);
  Buffer[1] := Hi(CRC16);
  // Создаем фрейм запроса и помещаем его в контейнер
  Frame     := WritePage(fSlaveId, CurrentPage, SizeOfPage, @Buffer, fTimeout);
  fPages.Add(Frame);

end;

procedure TBootloader1.CreateCommands;
begin
  fRunBootloader  := RunBootLoader(fSlaveId, fTimeout);
  fRunApplication := RunApplication(fSlaveId, fTimeout);
end;



procedure TBootloader1.Flash;
begin
  CreateCommands;
  CreatePages;
  if (fRunBootloader = nil) or (fRunApplication = nil) or (fPages.Count = 0) then
    raise Exception.Create('Ошибка создания запроса');

  fThreadFlash := TThreadFlash1.Create(Self);
  fThreadFlash.OnTerminate := fOnTerminate;
  fThreadFlash.Start;
end;

{ TThreadFlash }

constructor TThreadFlash.Create(const aBootloader: TBootloader);
begin
  inherited Create(True);
  fBootloader := aBootloader;
  fFlashing   := 0;
  FreeOnTerminate := False;
end;

procedure TThreadFlash.DoWrite;
begin
  fBootloader.fOnWrite(fBootloader.fPages.Count, fCurrentPage);
end;

procedure TThreadFlash.DoStatus;
begin
  fBootloader.fOnStatus(fStatus);
end;

function TThreadFlash.IsFlashing: Boolean;
begin
  Result := Windows.InterlockedCompareExchange(fFlashing, 0, 0) = 1;
end;

procedure TThreadFlash.Stop;
const
  TIMEOUT_CLOSE = 1000;
begin
  Windows.InterLockedExchange(fFlashing, 0);
  WaitForSingleObject(Handle, TIMEOUT_CLOSE);
end;



{ TThreadFlash1 }

function TThreadFlash1.RunBootloader(const aFrame: IFrame): Boolean;
const
  MB_DEVICE_FUNCTION: Byte = $68;
  MB_RUN_BOOTLOADER: Byte  = 2;

begin

  if Assigned(fBootloader.fOnStatus) then
  begin
    fStatus := 'Запуск Bootloader''a''';
    Synchronize(@DoStatus);
  end;

  Result := False;

  try
    while not fBootloader.fController.InQueue(aFrame) do
    begin
      sleep(20);
      if not IsFlashing then
        Exit;
    end;

    case aFrame.Responded of
      True:
      begin
        // Проверить ответ
        Result := (aFrame.ResponsePdu^[0] = MB_DEVICE_FUNCTION) and
          (aFrame.ResponsePdu^[1] = MB_RUN_BOOTLOADER) and
          (aFrame.ResponsePdu^[2] = 0);
        PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
      end;
      False:
        PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);
    end;

  finally
    if Assigned(fBootloader.fOnStatus) then
    begin
      case Result of
        True:
          fStatus := 'Bootloader запущен';
        False:
          fStatus := 'Ошибка запуска Bootloader''a''';
      end;
      Synchronize(@DoStatus);
    end;
  end;
end;

function TThreadFlash1.RunApplication(const aFrame: IFrame): Boolean;
const
  MB_DEVICE_FUNCTION: Byte = $68;
  MB_RUN_APPLICATION: Byte = 1;

begin

  if Assigned(fBootloader.fOnStatus) then
  begin
    fStatus := 'Запуск приложения';
    Synchronize(@DoStatus);
  end;

  Result := False;

  try
    while not fBootloader.fController.InQueue(aFrame) do
    begin
      sleep(20);
      if not IsFlashing then
        Exit;
    end;

    case aFrame.Responded of
      True:
      begin
        // Проверить ответ
        Result := (aFrame.ResponsePdu^[0] = MB_DEVICE_FUNCTION) and
          (aFrame.ResponsePdu^[1] = MB_RUN_APPLICATION) and
          (aFrame.ResponsePdu^[2] = 0);
        Result := True;
        PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
      end;
      False:
        PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);
    end;

  finally
    if Assigned(fBootloader.fOnStatus) then
    begin
      case Result of
        True:
          fStatus := 'Приложение запущено';
        False:
          fStatus := 'Ошибка запуска приложения';
      end;
      Synchronize(@DoStatus);
    end;
  end;

end;

function TThreadFlash1.WritePage(const aFrame: IFrame): Boolean;
const
  MB_FLASH_FUNCTION: Byte = $64;
begin

  Result := False;

  try
    while not fBootloader.fController.InQueue(aFrame) do
    begin
      sleep(20);
      if not IsFlashing then
        Exit;
    end;

    case aFrame.Responded of
      True:
      begin
        // Проверить ответ
        Result := (aFrame.ResponsePdu^[0] = MB_FLASH_FUNCTION);
        PostMessage(Application.MainFormHandle, WM_COUNT, 1, 0);
      end;
      False: PostMessage(Application.MainFormHandle, WM_COUNT, 0, 1);
    end;

  finally
    if Assigned(fBootloader.fOnWrite) then
    begin
      if Result then
        Inc(fCurrentPage);
      Synchronize(@DoWrite);
    end;

    //if Assigned(fBootloader.fOnStatus) then
    //begin
    //  fStatus := Format('%d из %d', [fCurrentPage, fBootloader.fPages.Count]);
    //  Synchronize(@DoStatus);
    //end;

  end;
end;



procedure TThreadFlash1.Execute;
var
  Bootloader: TBootloader1;
  Frame:      IFrame;
begin
  if not (fBootloader is TBootloader1) then
    Exit;
  BootLoader := TBootloader1(fBootLoader);

  Windows.InterLockedExchange(fFlashing, 1);
  try
    try

      // Запуск Bootloader
      while not RunBootloader(Bootloader.fRunBootLoader) do
      begin
        if not IsFlashing then
          Exit;
        Sleep(20);
      end;
      sleep(1000);


      // Загрузка прошивки
      if Assigned(fBootloader.fOnStatus) then
      begin
        fStatus := 'Старт прошивки...';
        Synchronize(@DoStatus);
      end;

      for Frame in Bootloader.fPages do
      begin
        while not WritePage(Frame) do
        begin
          if not IsFlashing then
            Exit;
          Sleep(20);
        end;
        if not IsFlashing then
          Exit;
      end;

      if Assigned(fBootloader.fOnStatus) then
      begin
        fStatus := 'Прошивание закончено';
        Synchronize(@DoStatus);
      end;


    finally
      sleep(1000);
      // Запуск приложения
      RunApplication(Bootloader.fRunApplication);
    end;
  finally
    Windows.InterLockedExchange(fFlashing, 0);
  end;

end;


end.








