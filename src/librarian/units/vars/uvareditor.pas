unit uVarEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees,
  uVtBaseEditor,
  uPropertyEditorNew,
  uLibrary;

type

  { TVarData }

  TVarData = class(TPropertyData)
  private
    fVar: IVarDefine;
  end;

  { TIndexVarData }

  TIndexVarData = class(TVarData)
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  end;

  { TNameVarData }

  TNameVarData = class(TVarData)
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  end;

  { TShortDescriptionVarData }

  TShortDescriptionVarData = class(TVarData)
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  end;

  { TTypeVarData }

  TTypeVarData = class(TVarData)
  private
    fStrings: TStrings;
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
    destructor Destroy; override;
  end;

  { TMeasureVarData }

  TMeasureVarData = class(TVarData)
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  end;


  { TMultiplerVarData }

  TMultiplerVarData = class(TVarData)
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  end;

  { TReadAlwaysVarData }

  TReadAlwaysVarData = class(TVarData)
  private
    fStrings: TStrings;
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
    destructor Destroy; override;
  end;

  { TAccessVarData }

  TAccessVarData = class(TVarData)
  private
    fStrings: TStrings;
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
    destructor Destroy; override;
  end;

  { TSynchronizationVarData }

  TSynchronizationVarData = class(TVarData)
  private
    fStrings: TStrings;
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
    destructor Destroy; override;
  end;

  { TKindVarData }

  TKindVarData = class(TVarData)
  private
    fStrings: TStrings;
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
    destructor Destroy; override;
  end;

   { TSingleRequestVarData }

  TSingleRequestVarData = class(TVarData)
  private
    fStrings: TStrings;
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
    destructor Destroy; override;
  end;

  { TVerVarData }

  TVerVarData = class(TVarData)
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  end;


  { TDescriptionVarData }

  TDescriptionVarData = class(TVarData)
  protected
    function GetValue : string; override;
    procedure SetValue(const aValue : string); override;
    function GetEditLink: IVtEditLink; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  end;


  { TVarEditor }

  TVarEditor = class(TPropertyEditor)
  private
    fVar: IVarDefine;
    procedure SetVar(const aVar: IVarDefine);
  public
    property VarDefine: IVarDefine read fVar write SetVar;
  end;

implementation

{ TDescriptionVarData }

function TDescriptionVarData.GetValue : string;
begin
  Result := fVar.Description.Text;
end;

procedure TDescriptionVarData.SetValue(const aValue : string);
begin
  fVar.Description.Text := aValue;
end;

function TDescriptionVarData.GetEditLink: IVtEditLink;
begin
  Result := TVtMemoEditLink.Create;
end;

constructor TDescriptionVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Описание';
  fVar := aVar;
end;

{ TVerVarData }

function TVerVarData.GetValue : string;
begin
  Result := fVar.Ver;
end;

procedure TVerVarData.SetValue(const aValue : string);
begin
  fVar.Ver := aValue;
end;

constructor TVerVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Версия';
  fVar := aVar;
end;

{ TSingleRequestVarData }

function TSingleRequestVarData.GetValue : string;
begin
   if fVar.SingleRequest then
    Result :='да'
  else
   Result :='нет';
end;

procedure TSingleRequestVarData.SetValue(const aValue : string);
begin
  fVar.SingleRequest := SameText(aValue, 'да');
end;

function TSingleRequestVarData.GetEditLink: IVtEditLink;
begin
  Result:= TVtComboEditLink.Create(fStrings);
end;

constructor TSingleRequestVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Единичный запрос';
  fStrings := TStringList.Create;
  fStrings.CommaText := 'да,нет';
  fVar := aVar;
end;

destructor TSingleRequestVarData.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;

{ TKindVarData }

function TKindVarData.GetValue : string;
begin
  Result := fStrings[Ord(fVar.Kind)];
end;

procedure TKindVarData.SetValue(const aValue : string);
begin
	fVar.Kind := TKind(fStrings.IndexOf(aValue));
end;

function TKindVarData.GetEditLink: IVtEditLink;
begin
  Result:= TVtComboEditLink.Create(fStrings);
end;

constructor TKindVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Тип переменной';
  fStrings := TStringList.Create;
  fStrings.CommaText := 'Обычная,Калибровочная';
  fVar := aVar;
end;

destructor TKindVarData.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;

{ TSynchronizationVarData }

function TSynchronizationVarData.GetValue : string;
begin
  Result := fStrings[Ord(fVar.Synchronization)];
end;

procedure TSynchronizationVarData.SetValue(const aValue : string);
begin
  fVar.Synchronization := TSynchronization(fStrings.IndexOf(aValue));
end;

function TSynchronizationVarData.GetEditLink: IVtEditLink;
begin
  Result:= TVtComboEditLink.Create(fStrings);
end;

constructor TSynchronizationVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Режим синхронизации';
  fStrings := TStringList.Create;
  fStrings.CommaText := 'Двунаправленный,Принудительный,Только запись';
  fVar := aVar;
end;

destructor TSynchronizationVarData.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;

{ TAccessVarData }

function TAccessVarData.GetValue : string;
begin
  Result := fStrings[Ord(fVar.Access)];
end;

procedure TAccessVarData.SetValue(const aValue : string);
begin
 fVar.Access := TAccess(fStrings.IndexOf(aValue));
end;

function TAccessVarData.GetEditLink: IVtEditLink;
begin
  Result:= TVtComboEditLink.Create(fStrings);
end;

constructor TAccessVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Уровень доступа';
  fStrings := TStringList.Create;
  fStrings.CommaText := 'Пользователь,Разработчик,Сервис';
  fVar := aVar;
end;

destructor TAccessVarData.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;

{ TReadAlwaysVarData }

function TReadAlwaysVarData.GetValue : string;
begin
  if fVar.ReadAlways then
    Result :='да'
  else
   Result :='нет';
end;

procedure TReadAlwaysVarData.SetValue(const aValue : string);
begin
  fVar.ReadAlways := SameText(aValue, 'да');
end;

function TReadAlwaysVarData.GetEditLink: IVtEditLink;
begin
  Result:= TVtComboEditLink.Create(fStrings);
end;

constructor TReadAlwaysVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Читать всегда';
  fStrings := TStringList.Create;
  fStrings.CommaText := 'да,нет';
  fVar := aVar;
end;

destructor TReadAlwaysVarData.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;

{ TMultiplerVarData }

function TMultiplerVarData.GetValue : string;
begin
  Result := IntToStr(fVar.Multipler);
end;

procedure TMultiplerVarData.SetValue(const aValue : string);
begin
  fVar.Multipler := StrToIntDef(aValue, 0);
end;

function TMultiplerVarData.GetEditLink: IVtEditLink;
begin
  Result:= TVtIntegerEditLink.Create(1, 1000000, 1);
end;

constructor TMultiplerVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Множитель';
  fVar := aVar;
end;

{ TMeasureVarData }

function TMeasureVarData.GetValue : string;
begin
  Result := fVar.Measure;
end;

procedure TMeasureVarData.SetValue(const aValue : string);
begin
  fVar.Measure := aValue;
end;

constructor TMeasureVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Единица измерения';
  fVar := aVar;
end;

{ TTypeVarData }

function TTypeVarData.GetValue : string;
var
  FoundIndex: Integer;
begin
  FoundIndex := Vartypes.IndexOfData(fVar.VarType);
  Result := VarTypes.Keys[FoundIndex];
end;

procedure TTypeVarData.SetValue(const aValue : string);
begin
  fVar.VarType := VarTypes[aValue];
end;

function TTypeVarData.GetEditLink: IVtEditLink;
begin
  Result := TVtComboEditLink.Create(fStrings);
end;

constructor TTypeVarData.Create(const aVar : IVarDefine);
var
  I: Integer;
begin
  inherited Create;
  Key := 'Тип данных';
  fVar := aVar;
  fStrings := TStringList.Create;
  for I := 0 to VarTypes.Count - 1 do
    fStrings.Add(VarTypes.Keys[I]);
end;

destructor TTypeVarData.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;

{ TShortDescriptionVarData }

function TShortDescriptionVarData.GetValue : string;
begin
  Result := fVar.ShortDescription;
end;

procedure TShortDescriptionVarData.SetValue(const aValue : string);
begin
 fVar.ShortDescription := aValue;
end;

constructor TShortDescriptionVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Краткое описание';
  fVar := aVar;
end;

{ TNameVarData }

function TNameVarData.GetValue : string;
begin
  Result := fVar.Name;
end;

procedure TNameVarData.SetValue(const aValue : string);
begin
  fVar.Name := aValue
end;

constructor TNameVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Название переменной';
  fVar := aVar;
end;

{ TIndexVarData }

constructor TIndexVarData.Create(const aVar : IVarDefine);
begin
  inherited Create;
  Key := 'Индекс';
  fVar := aVar;
end;

function TIndexVarData.GetValue : string;
begin
  Result := IntToStr(fVar.Index);
end;

procedure TIndexVarData.SetValue(const aValue : string);
begin
  fVar.Index := StrToIntDef(aValue, 0);
end;

function TIndexVarData.GetEditLink: IVtEditLink;
begin
  Result := TVtIntegerEditLink.Create(0, 65535, 1);
end;

{ TVarEditor }

procedure TVarEditor.SetVar(const aVar: IVarDefine);
var
  Node: PVirtualNode;
begin
  if fVar <> aVar then
    fVar := aVar;

  BeginUpdate;
  try
    Clear;

    // Описание переменной
    AddChild(nil, TIndexVarData.Create(aVar));
    AddChild(nil, TNameVarData.Create(aVar));
    AddChild(nil, TShortDescriptionVarData.Create(aVar));
    AddChild(nil, TMeasureVarData.Create(aVar));
    AddChild(nil, TTypeVarData.Create(aVar));
    AddChild(nil, TKindVarData.Create(aVar));
    AddChild(nil, TMultiplerVarData.Create(aVar));
    AddChild(nil, TReadAlwaysVarData.Create(aVar));
    AddChild(nil, TAccessVarData.Create(aVar));
    AddChild(nil, TSynchronizationVarData.Create(aVar));
    AddChild(nil, TSingleRequestVarData.Create(aVar));
    Node := AddChild(nil, TDescriptionVarData.Create(aVar));

    Node^.NodeHeight := 3 * Node^.NodeHeight;
  	Node^.States := Node^.States + [vsMultiline];

  finally
  	EndUpdate;
  end;


end;

end.

