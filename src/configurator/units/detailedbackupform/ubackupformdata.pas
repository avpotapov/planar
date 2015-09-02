unit uBackupFormData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees,
  uLibrary;

type

  { TBaseBackupData }

  TBaseBackupData = class
    type
    // Типы иконок
    TTypeImage = (iiNone = -1, iiInactive, iiActive);
  protected
    function GetCaption: String; virtual; abstract;
  public
    function GetImage: TTypeImage; virtual; abstract;
    function GetSelected: TTypeImage; virtual; abstract;
    property Caption: String read GetCaption;
  end;

  { TGroupsBackupData }

  TGroupsBackupData = class(TBaseBackupData)
  private
    fGroups: IGroups;
  public
    constructor Create(const aGroups: IGroups); reintroduce;

  protected
    function GetCaption: String; override;

  public
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
  end;

  { TGroupItemBackupData }

  TGroupItemBackupData = class(TBaseBackupData)
  private
    fGroupItem: IGroupItem;
    fChecked:   Boolean;
    fOwnerNode: PVirtualNode;
    fTree:      TBaseVirtualTree;
  public
    constructor Create(const aGroupItem: IGroupItem); reintroduce;

  protected
    function GetCaption: String; override;
    procedure SetChecked(const aChecked: Boolean);

  public
    function GetImage: TTypeImage; override;
    function GetSelected: TTypeImage; override;
    property Checked: Boolean read fChecked write SetChecked;
    property OwnerNode: PVirtualNode read fOwnerNode write fOwnerNode;
    property Tree: TBaseVirtualTree read fTree write fTree;
  end;


// Для сортировки переменных группы
function GroupItemCompare(const Item1, Item2: TGroupItemBackupData): Integer;

implementation

function GroupItemCompare(const Item1, Item2: TGroupItemBackupData): Integer;
begin
  Result := Item1.fGroupItem.VarDefine.Index - Item2.fGroupItem.VarDefine.Index;
  if Result = 0 then
    Result := Ord(Item1.fGroupItem.VarDefine.VarType) -
      Ord(Item2.fGroupItem.VarDefine.VarType);
end;

{ TGroupsBackupData }

constructor TGroupsBackupData.Create(const aGroups: IGroups);
begin
  inherited Create;
  fGroups := aGroups;
end;

function TGroupsBackupData.GetCaption: String;
begin
  Result := fGroups.ShortDescription;
end;

function TGroupsBackupData.GetImage: TTypeImage;
begin
  Result := TTypeImage.iiInActive;
end;

function TGroupsBackupData.GetSelected: TTypeImage;
begin
  Result := TTypeImage.iiActive;
end;

{ TGroupItemBackupData }

procedure TGroupItemBackupData.SetChecked(const aChecked: Boolean);
begin
  if fChecked = aChecked then
    Exit;
  fChecked := aChecked;
  if (fTree <> nil) and (fOwnerNode <> nil) then
    if fOwnerNode^.CheckType = TCheckType.ctCheckBox then
    begin
      case fChecked of
        True: fOwnerNode^.CheckState  := TCheckState.csCheckedNormal;
        False: fOwnerNode^.CheckState := TCheckState.csUncheckedNormal;
      end;
      fTree.InvalidateNode(fOwnerNode);
    end;
end;

constructor TGroupItemBackupData.Create(const aGroupItem: IGroupItem);
begin
  inherited Create;
  fGroupItem := aGroupItem;
  fChecked   := False;
end;

function TGroupItemBackupData.GetCaption: String;
var
  C: Char;
begin
  case fGroupItem.VarDefine.TypeRegister of
    trHolding: C := 'h';
    trInput: C   := 'i';
  end;
  Result := Format('[%s %d] %s', [C, fGroupItem.VarDefine.Index,
    fGroupItem.VarDefine.ShortDescription]);
end;

function TGroupItemBackupData.GetImage: TTypeImage;
begin
  Result := TTypeImage.iiNone;
end;

function TGroupItemBackupData.GetSelected: TTypeImage;
begin
  Result := TTypeImage.iiNone;
end;

end.
