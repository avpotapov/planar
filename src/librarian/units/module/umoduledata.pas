unit uModuleData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Windows, Forms,
  uPropertyEditorNew,
  uVtBaseEditor,
  uLibrary;

type

  { TModuleData }

  TModuleData = class(TPropertyData)
  protected
    fModule: IModule;
  end;

  { TUidData }

  TUidData = class(TModuleData)
  protected
    function GetValue: String; override;
    procedure SetValue(const aValue: String); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aModule: IModule); reintroduce;
  end;

  { TNameData }

  TNameData = class(TModuleData)
  protected
    function GetValue: String; override;
    procedure SetValue(const aValue: String); override;
  public
    constructor Create(const aModule: IModule); reintroduce;
  end;

  { TTypeSignatureData }

  TTypeSignatureData = class(TModuleData)
  private
    fStrings: TStrings;
  protected
    function GetValue: String; override;
    procedure SetValue(const aValue: String); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aModule: IModule); reintroduce;
    destructor Destroy; override;
  end;


  { TTypeBootloaderData }

  TTypeBootloaderData = class(TModuleData)
  private
    fStrings: TStrings;
  protected
    function GetValue: String; override;
    procedure SetValue(const aValue: String); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aModule: IModule); reintroduce;
    destructor Destroy; override;
  end;

  { TModuleEditor }

  TModuleEditor = class(TPropertyEditor)
  private
    fModule: IModule;
    procedure SetModule(const aModule: IModule);
  public
    property Module: IModule read fModule write SetModule;
  end;



implementation

{ TModuleEditor }

procedure TModuleEditor.SetModule(const aModule: IModule);
begin
  if fModule <> aModule then
  	fModule := aModule;
  Clear;

  AddChild(nil, TUidData.Create(fModule));
  AddChild(nil, TNameData.Create(fModule));
  AddChild(nil, TTypeSignatureData.Create(fModule));
  AddChild(nil, TTypeBootloaderData.Create(fModule));

end;

{ TUidData }

constructor TUidData.Create(const aModule: IModule);
begin
  inherited Create;
  Key := 'UID';
  fModule := aModule;
end;

function TUidData.GetValue: String;
begin
  Result := IntToStr(fModule.Uid);
end;

procedure TUidData.SetValue(const aValue: String);
begin
  try
    fModule.Uid :=StrToIntDef(aValue, 0);
    if fModule.GetLastError <> 0 then
      raise Exception.Create('Модуль с данным Uid уже существует');
  except
    on E: Exception do
      MessageBox(Application.MainFormHandle, PChar(Utf8ToAnsi(E.Message)),
        PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
  end;
end;

function TUidData.GetEditLink: IVtEditLink;
begin
  Result	:= TVtIntegerEditLink.Create(1, 65535, 1);
end;

{ TNameData }

constructor TNameData.Create(const aModule: IModule);
begin
  inherited Create;
  Key := 'Название';
  fModule := aModule;
end;

function TNameData.GetValue: String;
begin
  Result := fModule.Name;
end;

procedure TNameData.SetValue(const aValue: String);
begin
  fModule.Name := aValue;
end;

{ TTypeSignatureData }

constructor TTypeSignatureData.Create(const aModule: IModule);
begin
  inherited Create;
  Key := 'Тип сигнатуры';
  fModule := aModule;
  fStrings := TStringList.Create;
  fStrings.CommaText := 'Нет,Автоопределение,RCCU3.0';
end;

destructor TTypeSignatureData.Destroy;
begin
 FreeAndNil(fStrings);
 inherited Destroy;
end;

function TTypeSignatureData.GetValue: String;
begin
 Result := fStrings[Ord(fModule.TypeSignature)];
end;

procedure TTypeSignatureData.SetValue(const aValue: String);
begin
 fModule.TypeSignature := TTypeSignature(fStrings.IndexOf(aValue));
end;

function TTypeSignatureData.GetEditLink: IVtEditLink;
begin
  Result := TVtComboEditLink.Create(fStrings);
end;

{ TTypeBootloaderData }

function TTypeBootloaderData.GetValue: String;
begin
 Result := fStrings[Ord(fModule.TypeBootloader)];
end;

procedure TTypeBootloaderData.SetValue(const aValue: String);
begin
 fModule.TypeBootloader := TTypeBootloader(fStrings.IndexOf(aValue));
end;

function TTypeBootloaderData.GetEditLink: IVtEditLink;
begin
 Result := TVtComboEditLink.Create(fStrings);
end;

constructor TTypeBootloaderData.Create(const aModule: IModule);
begin
 inherited Create;
 Key := 'Тип bootloader''a''';
 fModule := aModule;
 fStrings := TStringList.Create;
 fStrings.CommaText := '1,2,3';
end;

destructor TTypeBootloaderData.Destroy;
begin
 FreeAndNil(fStrings);
 inherited Destroy;
end;

end.

