unit uSettingFormData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uCustomEditor, upropertyeditor;

type
   {$REGION Settings}

  { TTreeData }

  TTreeData = class(TPropertyData)
  type
    TTypeImage = (tiNone = -1, tiModbus, tiRtu, tiClient, tiServer,
      tiConfig, tiTimeout, tiXmlLibrary, tiDeveloper, tiUser);
  private
    fImage: TTypeImage;
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aCaption: string; const aImage: TTypeImage); reintroduce;
  public
    property Image: TTypeImage read fImage write fImage;
  end;

  { TCommaTextData }

  TCommaTextData = class(TPropertyData)
  public
    constructor Create(const aCaption: String;
      const aCommaText: String); reintroduce;
  end;


  { TBaudRate }

  TBaudRate = class(TCommaTextData)
  protected
    procedure SetValue(const aValue: string); override;
    function GetValue: string;  override;
  public
    constructor Create; reintroduce;
  end;

  { TParity }

  TParity = class(TCommaTextData)
  protected
    procedure SetValue(const aValue: string); override;
    function GetValue: string;  override;
  public
    constructor Create; reintroduce;
  end;

  { TStopBits }

  TStopBits = class(TCommaTextData)
  protected
    procedure SetValue(const aValue: string); override;
    function GetValue: string;  override;
  public
    constructor Create; reintroduce;
  end;

 { TByteSize }

 TByteSize = class(TCommaTextData)
  protected
    procedure SetValue(const aValue: string); override;
    function GetValue: string;  override;
  public
    constructor Create; reintroduce;
  end;

 { TThreeAndHalf }

 TThreeAndHalf = class(TPropertyData)
  protected
    procedure SetValue(const aValue: string); override;
    function GetValue: string;  override;
  public
    constructor Create; reintroduce;
  end;

 { TIpAddress }

 TIpAddress = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TPort }

 TPort = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TServerPort }

 TServerPort = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TAddress }

 TAddress = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TKeepAlive }

 TKeepAlive = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TTimeout }

 TTimeout = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TResponseTimeout }

 TResponseTimeout = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TBackupTimeout }

 TBackupTimeout = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TRepeats }

 TRepeats = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TDeveloperLibrary }

 TDeveloperLibrary = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TDeveloperImage }

 TDeveloperImage = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;


 { TUserLibrary }

 TUserLibrary = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

 { TUserImage }

 TUserImage = class(TPropertyData)
 protected
   procedure SetValue(const aValue: string); override;
   function GetValue: string;  override;
 public
   constructor Create; reintroduce;
 end;

{$ENDREGION Settings}

implementation
uses
  uSetting;

{$REGION Settings}
{ TTreeData }

function TTreeData.GetValue: string;
begin
  Result := '';
end;

procedure TTreeData.SetValue(const aValue: string);
begin
{ TODO : Stub }
end;

constructor TTreeData.Create(const aCaption: string; const aImage: TTypeImage);
begin
  inherited Create(aCaption);
  fImage := aImage;
end;

{ TCommaTextData }

constructor TCommaTextData.Create(const aCaption: String;
  const aCommaText: String);
begin
  inherited Create(aCaption, TBaseData.TTypeEditor.teComboBox);
  Strings.CommaText := aCommaText;
end;

{ TUserImage }

procedure TUserImage.SetValue(const aValue: string);
begin
  GetSetting.UserImage := Utf8ToAnsi(aValue);
end;

function TUserImage.GetValue: string;
begin
  Result := AnsiToUtf8(GetSetting.UserImage);
end;

constructor TUserImage.Create;
begin
  inherited Create('Image');
  TypeEditor := TBaseData.TTypeEditor.teDirectoryEdit;
end;

{ TUserLibrary }

procedure TUserLibrary.SetValue(const aValue: string);
begin
  GetSetting.UserLibrary := Utf8ToAnsi(aValue);
end;

function TUserLibrary.GetValue: string;
begin
  Result := AnsiToUtf8(GetSetting.UserLibrary);
end;

constructor TUserLibrary.Create;
begin
  inherited Create('XML');
  TypeEditor := TBaseData.TTypeEditor.teDirectoryEdit;
end;

{ TDeveloperImage }

procedure TDeveloperImage.SetValue(const aValue: string);
begin
  GetSetting.DeveloperImage := Utf8ToAnsi(aValue);
end;

function TDeveloperImage.GetValue: string;
begin
  Result := AnsiToUtf8(GetSetting.DeveloperImage);
end;

constructor TDeveloperImage.Create;
begin
  inherited Create('Image');
  TypeEditor := TBaseData.TTypeEditor.teDirectoryEdit;
end;

{ TDeveloperLibrary }

procedure TDeveloperLibrary.SetValue(const aValue: string);
begin
  GetSetting.DeveloperLibrary := Utf8ToAnsi(aValue);
end;

function TDeveloperLibrary.GetValue: string;
begin
  Result := AnsiToUtf8(GetSetting.DeveloperLibrary);
end;

constructor TDeveloperLibrary.Create;
begin
  inherited Create('XML');
  TypeEditor := TBaseData.TTypeEditor.teDirectoryEdit;
end;

{ TRepeats }

procedure TRepeats.SetValue(const aValue: string);
begin
  GetSetting.Repeats := StrToIntDef(aValue, 0);
end;

function TRepeats.GetValue: string;
begin
  Result := IntToStr(GetSetting.Repeats);
end;

constructor TRepeats.Create;
begin
  inherited Create('Число повторов');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TBackupTimeout }

procedure TBackupTimeout.SetValue(const aValue: string);
begin
  GetSetting.BackupTimeout := StrToIntDef(aValue, 1000);
end;

function TBackupTimeout.GetValue: string;
begin
  Result := IntToStr(GetSetting.BackupTimeout);
end;

constructor TBackupTimeout.Create;
begin
  inherited Create('Таймаут архивации');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TResponseTimeout }

procedure TResponseTimeout.SetValue(const aValue: string);
begin
  GetSetting.ResponseTimeout := StrToIntDef(aValue, 0);
end;

function TResponseTimeout.GetValue: string;
begin
  Result := IntToStr(GetSetting.ResponseTimeout);
end;

constructor TResponseTimeout.Create;
begin
  inherited Create('Таймаут ответа при автопоиске');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TTimeout }

procedure TTimeout.SetValue(const aValue: string);
begin
  GetSetting.Timeout := StrToIntDef(aValue, 1000);
end;

function TTimeout.GetValue: string;
begin
  Result := IntToStr(GetSetting.Timeout);
end;

constructor TTimeout.Create;
begin
  inherited Create('Таймаут ответа');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TKeepAlive }

procedure TKeepAlive.SetValue(const aValue: string);
begin
  GetSetting.KeepAliveTimeout := StrToIntDef(aValue, 5000);
end;

function TKeepAlive.GetValue: string;
begin
  Result := IntToStr(GetSetting.KeepAliveTimeout);
end;

constructor TKeepAlive.Create;
begin
  inherited Create('Keep Alive');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TAddress }

procedure TAddress.SetValue(const aValue: string);
begin
  GetSetting.KeepAliveAddress := StrToIntDef(aValue, 246);
end;

function TAddress.GetValue: string;
begin
  Result := IntTOStr(GetSetting.KeepAliveAddress);
end;

constructor TAddress.Create;
begin
  inherited Create('Адрес');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TServerPort }

procedure TServerPort.SetValue(const aValue: string);
begin
  GetSetting.ServerPort := StrToIntDef(aValue, 502);
end;

function TServerPort.GetValue: string;
begin
  Result := IntToStr(GetSetting.ServerPort);
end;

constructor TServerPort.Create;
begin
  inherited Create('Порт');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TPort }

procedure TPort.SetValue(const aValue: string);
begin
  GetSetting.Port := StrToIntDef(aValue, 502);
end;

function TPort.GetValue: string;
begin
  Result := IntToStr(GetSetting.Port);
end;

constructor TPort.Create;
begin
  inherited Create('Порт');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TIpAddress }

procedure TIpAddress.SetValue(const aValue: string);
begin
  GetSetting.Ip := aValue;
end;

function TIpAddress.GetValue: string;
begin
  Result := GetSetting.Ip;
end;

constructor TIpAddress.Create;
begin
  inherited Create('IP-адрес');
  TypeEditor := TBaseData.TTypeEditor.teEdit ;
end;

{ TThreeAndHalf }

procedure TThreeAndHalf.SetValue(const aValue: string);
begin
  GetSetting.ThreeAndHalf := StrToIntDef(aValue, 30);
end;

function TThreeAndHalf.GetValue: string;
begin
  Result := IntToStr(GetSetting.ThreeAndHalf);
end;

constructor TThreeAndHalf.Create;
begin
  inherited Create('Таймаут в 3,5 байта');
  TypeEditor := TBaseData.TTypeEditor.teSpinEdit;
end;

{ TByteSize }

procedure TByteSize.SetValue(const aValue: string);
begin
  GetSetting.ByteSize := StrToInt(aValue);
end;

function TByteSize.GetValue: string;
begin
  Result := IntToStr(GetSetting.ByteSize);
end;

constructor TByteSize.Create;
begin
  inherited Create('Число бит в байте', '6,7,8');
end;

{ TStopBits }

procedure TStopBits.SetValue(const aValue: string);
begin
  GetSetting.StopBits := StrToInt(aValue);
end;

function TStopBits.GetValue: string;
begin
  Result := IntTOStr(GetSetting.StopBits);
end;

constructor TStopBits.Create;
begin
  inherited Create('Количество стоповых бит', '0,1,2');
end;

{ TParity }

procedure TParity.SetValue(const aValue: string);
begin
  GetSetting.Parity := StrToInt(aValue);
end;

function TParity.GetValue: string;
begin
  Result := IntToStr(GetSetting.Parity);
end;

constructor TParity.Create;
begin
  inherited Create('Режим проверки четности', '0,1,2,3');
end;

{ TBaudRate }
constructor TBaudRate.Create;
begin
  inherited Create('Скорость передачи данных',
    '1200,2400,4800,9600,14400,19200,38400,56000,57600,115200,128000,256000');
end;


function TBaudRate.GetValue: string;
begin
  Result := IntToStr(GetSetting.BaudRate);
end;


procedure TBaudRate.SetValue(const aValue: string);
begin
  GetSetting.BaudRate := StrToInt(aValue);
end;
{$ENDREGION Settings}
end.

