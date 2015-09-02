unit uDeviceFormData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Forms,
  upropertyeditor,
  uConfiguratorData;

type
  { TDeviceBaseData }

  TDeviceBaseData = class(TPropertyData)
  private
    fDeviceData: TDeviceData;
    fReadOnly: Boolean;
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aCaption: string; const aDeviceData: TDeviceData); reintroduce;
  public
    property ReadOnly: Boolean read freadonly write fReadOnly;
  end;

  { TModuleData }

  TModuleData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
    procedure AfterConstruction; override;
  end;

  { TSlaveIdData }

  TSlaveIdData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
    procedure AfterConstruction; override;
  end;

  { TTypeIdHardData }

  TTypeIdHardData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;

  { TModificatorData }

  TModificatorData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;


  { TSerialNumberData }

  TSerialNumberData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;

  { TDateIdHardData }

  TDateIdHardData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;


  { TTypeFirmWareData }

  TTypeFirmWareData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;


  { TVersionFirmWareData }

  TVersionFirmWareData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;



  { TTypeProjectData }

  TTypeProjectData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;



  { TVersionProjectData }

  TVersionProjectData = class(TDeviceBaseData)
  protected
    function GetValue: string; override;
    procedure SetValue(const {%H-}aValue: string); override;
  public
    constructor Create(const aDeviceData: TDeviceData); reintroduce;
  end;


implementation
uses
  uSetting,
  uLibrary;

{ TVersionProjectData }

constructor TVersionProjectData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Версия проекта', aDeviceData);
end;

function TVersionProjectData.GetValue: string;
begin
  Result:= fDeviceData.Signarute.VersionProject;

end;

procedure TVersionProjectData.SetValue(const aValue: string);
begin
  { TODO : Stub }
end;


{ TTypeProjectData }

constructor TTypeProjectData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Тип проекта', aDeviceData);
end;

function TTypeProjectData.GetValue: string;
begin
  Result := IntToStr(fDeviceData.Signarute.TypeProject);
end;

procedure TTypeProjectData.SetValue(const aValue: string);
begin
  { TODO : Stub }
end;

{ TVersionFirmWareData }

constructor TVersionFirmWareData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Версия прошивки', aDeviceData);
end;

function TVersionFirmWareData.GetValue: string;
begin
  Result:= fDeviceData.Signarute.VersionFirmWare;
end;

procedure TVersionFirmWareData.SetValue(const aValue: string);
begin
  { TODO : Stub }
end;



{ TTypeFirmWareData }

constructor TTypeFirmWareData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Тип прошивки', aDeviceData);
end;

function TTypeFirmWareData.GetValue: string;
begin
  Result:= IntToStr(fDeviceData.Signarute.TypeFirmWare);
end;

procedure TTypeFirmWareData.SetValue(const aValue: string);
begin
{ TODO : Stub }
end;

{ TDateIdHardData }

constructor TDateIdHardData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Дата изготовления', aDeviceData);
end;

function TDateIdHardData.GetValue: string;
begin
  Result := fDeviceData.Signarute.DateIdHard;
end;

procedure TDateIdHardData.SetValue(const aValue: string);
begin
{ TODO : Stub }
end;


{ TSerialNumberData }

constructor TSerialNumberData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Серийный номер', aDeviceData);
end;


function TSerialNumberData.GetValue: string;
begin
  Result := fDeviceData.Signarute.SerialNumber;
end;

procedure TSerialNumberData.SetValue(const aValue: string);
begin
{ TODO : Stub }
end;


{ TModificatorData }

constructor TModificatorData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Модификатор', aDeviceData);
end;


function TModificatorData.GetValue: string;
begin
  Result := IntToStr(fDeviceData.Signarute.Modificator);
end;

procedure TModificatorData.SetValue(const aValue: string);
begin
{ TODO : Stub }
end;


{ TTypeIdHardData }

constructor TTypeIdHardData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Тип платформы', aDeviceData);
end;

function TTypeIdHardData.GetValue: string;
begin
  Result := IntToStr(fDeviceData.Signarute.TypeIdHard);
end;

procedure TTypeIdHardData.SetValue(const aValue: string);
begin
{ TODO : Stub }
end;

{ TSlaveIdData }
constructor TSlaveIdData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Адрес устройства', aDeviceData);
  TypeEditor := TTypeEditor.teComboBox;
  fReadOnly := False;
end;

procedure TSlaveIdData.AfterConstruction;
var
  I: Integer;
begin
  inherited AfterConstruction;

  for I := 1 to 247 do
     Strings.AddObject(IntToStr(I), TObject(I));
end;

function TSlaveIdData.GetValue: string;
begin
  Result := IntToStr(fDeviceData.SlaveId);
end;

procedure TSlaveIdData.SetValue(const aValue: string);
begin
  fDeviceData.SlaveId := StrToInt(aValue);
end;

{ TModuleData }

constructor TModuleData.Create(const aDeviceData: TDeviceData);
begin
  inherited Create('Модуль устройства', aDeviceData);
  TypeEditor := TTypeEditor.teComboBox;
  fReadOnly := False;
end;

procedure TModuleData.AfterConstruction;
var
  Lib: ILibrary;
  Sublib: ISublibrary;
  M: IModule;
  P, P1: Pointer;
begin
  inherited AfterConstruction;

  try

    Strings.Add('Неизвестное устройство');

    // Загрузить библиотеку
    Lib := GetLibrary([
      GetSetting.DeveloperLibrary + '\module.jlf',
      GetSetting.UserLibrary + '\module.jlf']);

    for P in Lib do
    begin
      Sublib := Lib.ExtractData(P);
      for P1 in Sublib do
      begin
         M := Sublib.ExtractData(P1);
         Strings.AddObject(M.Name, TObject(Pointer(M)));
      end;
    end;

  except
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi('Ошибка загрузки списка модулей')),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;

end;

function TModuleData.GetValue: string;
begin
  if  fDeviceData.Module = nil then
    Result := 'Неизвестное устройство'
  else
    Result := fDeviceData.Module.Name;
end;

procedure TModuleData.SetValue(const aValue: string);
var
  FoundIndex: Integer;
begin
  FoundIndex := Strings.IndexOf(aValue);
  if FoundIndex <= 0 then
    fDeviceData.Module := nil
  else
    fDeviceData.Module := IModule(Pointer(Strings.Objects[FoundIndex]));
end;

{ TDeviceBaseData }

function TDeviceBaseData.GetValue: string;
begin
  Result := '';
end;

procedure TDeviceBaseData.SetValue(const aValue: string);
begin

end;

constructor TDeviceBaseData.Create(const aCaption: string;
  const aDeviceData: TDeviceData);
begin
  inherited Create(aCaption);
  fDeviceData := aDeviceData;
  fReadOnly := True;
end;



end.

