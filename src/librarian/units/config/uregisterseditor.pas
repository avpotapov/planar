unit uregisterseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ImgList, Controls,
  uLibrary;

type

  { TRegisterData }

  TRegisterData = class
  protected
    function GetCaption: string; virtual; abstract;
    function GetImageIndex: integer; virtual;
  public
    property Caption: string read GetCaption;
    property ImageIndex: integer read GetImageIndex;
  end;


  { TVarsData }

  TVarsData = class(TRegisterData)
  private
    fVars: IVars;
    fTypeRegister: TTypeRegister;
  protected
    function GetCaption: string; override;
  public
    constructor Create(const aTypeRegister: TTypeRegister; const aVars: IVars); reintroduce;
  end;


  { TVarData }

  TVarData = class(TRegisterData)
  private
    fVar: IVarDefine;
  protected
    function GetCaption: string; override;
  public
    constructor Create(const aVar: IVarDefine); reintroduce;
  public
    property VarDefine: IVarDefine read fVar;
  end;


  TNodeSelectEvent = procedure(Sender: TObject; const aRegister: TRegisterData) of object;

  { TRegistersEditor }

  TRegistersEditor = class(TVirtualStringTree)
  private
    fOnNodeSelect: TNodeSelectEvent;


  protected
    function DoCompare(Node1, Node2: PVirtualNode; {%H-}Column: TColumnIndex): integer;
      override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoChange(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    {%H-}TextType: TVSTTextType; var CellText: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: boolean; var Index: integer): TCustomImageList;
      override;

    function DoBeforeDrag(Node: PVirtualNode; {%H-}Column: TColumnIndex): boolean; override;
    function DoDragOver(Source: TObject; {%H-}Shift: TShiftState;
      {%H-}State: TDragState; const {%H-}Pt: TPoint; {%H-}Mode: TDropMode;
      var {%H-}Effect: longword): boolean;
      override;
  public
    function GetData(const aNode: PVirtualNode): TRegisterData; virtual;
    procedure LoadRegisters(const aRegisters: IRegisters);
    procedure AfterConstruction; override;
    property OnNodeSelect: TNodeSelectEvent read fOnNodeSelect write fOnNodeSelect;
  end;



implementation

{ TVarData }

function TVarData.GetCaption: string;
begin
  case fVar.VarType of
    TVarType.vtUINT8H: Result := Format('[%dh] %s', [fVar.Index, fVar.ShortDescription]);
    TVarType.vtUINT8L: Result := Format('[%dl] %s', [fVar.Index, fVar.ShortDescription]);
  else
    Result := Format('[%d] %s', [fVar.Index, fVar.ShortDescription]);
  end;
end;

constructor TVarData.Create(const aVar: IVarDefine);
begin
  inherited Create;
  fVar := aVar;
end;

{ TVarsData }

function TVarsData.GetCaption: string;
begin
	case fTypeRegister of
 		trHolding: Result := 'HOLDING';
 		trInput: Result := 'INPUT';
 end;
end;

constructor TVarsData.Create(const aTypeRegister: TTypeRegister; const aVars: IVars);
begin
  inherited Create;
  fVars := aVars;
  fTypeRegister := aTypeRegister;
end;

{ TRegisterData }

function TRegisterData.GetImageIndex: integer;
begin
  Result := -1;
end;

{ TRegistersEditor }
procedure TRegistersEditor.LoadRegisters(const aRegisters: IRegisters);
var
  P, PP: Pointer;
  Node: PVirtualNode;
  Vars: IVars;
begin
  {$IFDEF DEBUG}
  Assert(aRegisters <> nil);
  {$ENDIF}

  for P in aRegisters do
  begin
    Vars := aRegisters.ExtractData(P);
    Node := AddChild(nil, TVarsData.Create(aRegisters.ExtractKey(P), Vars));
    for PP in Vars do
    begin
    	AddChild(Node, TVarData.Create(Vars.ExtractData(PP)));
    end;
  end;

  Header.Treeview.SortTree(0, sdAscending, False);
  Header.SortColumn := 0;
  Header.SortDirection := sdAscending;

end;


function TRegistersEditor.GetData(const aNode: PVirtualNode): TRegisterData;
var
  P: Pointer;
begin
  Result := nil;
  if aNode <> nil then
  begin
    P := GetNodeData(aNode);
    Result := TRegisterData(P^);
  end;
end;

function TRegistersEditor.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): integer;
begin
	if (GetData(Node1) is TVarData) and (GetData(Node2) is TVarData) then
  begin
    Result := TVarData(GetData(Node1)).fVar.Index - TVarData(GetData(Node2)).fVar.Index;
  end;
end;

procedure TRegistersEditor.AfterConstruction;
begin
  inherited AfterConstruction;

  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus,
  	toFullRowSelect, toRightClickSelect, toMultiSelect];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines] +
    [toShowVertGridLines, toShowHorzGridLines];
  LineStyle := lsSolid;
  TreeOptions.AutoOptions := TreeOptions.AutoOptions - [toAutoDeleteMovedNodes];

  DefaultNodeHeight := 24;

  NodeDataSize := SizeOf(TRegisterData);
end;

procedure TRegistersEditor.DoFreeNode(Node: PVirtualNode);
begin
  GetData(Node).Free;
  inherited DoFreeNode(Node);
end;

procedure TRegistersEditor.DoChange(Node: PVirtualNode);
begin
  inherited DoChange(Node);
  if Node <> nil then
  	if Assigned(fOnNodeSelect) then
    	fOnNodeSelect(Self, GetData(Node));
end;

procedure TRegistersEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  CellText := GetData(Node).Caption;
  inherited DoGetText(Node, Column, TextType, CellText);
end;

function TRegistersEditor.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean;
  var Index: integer): TCustomImageList;
begin
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
  Index := GetData(Node).ImageIndex;
end;

function TRegistersEditor.DoBeforeDrag(Node: PVirtualNode; Column: TColumnIndex
  ): boolean;
begin
  Result := GetData(Node) is TVarData;
end;

function TRegistersEditor.DoDragOver(Source: TObject; Shift: TShiftState;
  State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: longword
  ): boolean;
begin
  Result := Source = Self;
end;

end.

