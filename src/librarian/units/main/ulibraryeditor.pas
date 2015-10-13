unit uLibraryEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ImgList,
  uSetting,
  uLibrary,
  uLibraryData;

type
  TNodeSelectEvent = procedure(Sender: TObject; const aLibraryData: TLibraryData) of object;


  { TLibraryEditor }

  TLibraryEditor = class(TVirtualStringTree)
  private
    fLibrary: ILibrary;
  private
    fOnNodeSelect: TNodeSelectEvent;
    function GetData(const aNode: PVirtualNode): TLibraryData; virtual;

  protected
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoChange(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    {%H-}TextType: TVSTTextType; var CellText: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: boolean; var Index: integer): TCustomImageList;
      override;
  public
    function GetLibrary: ILibrary;
    procedure AfterConstruction; override;
    procedure LoadLibrary;
    property OnNodeSelect: TNodeSelectEvent read fOnNodeSelect write fOnNodeSelect;
  end;

implementation
uses
    uLibraryBuilder;



{ TLibraryEditor }

function TLibraryEditor.GetData(const aNode: PVirtualNode): TLibraryData;
var
  P: Pointer;
begin
  Result := nil;
  if aNode <> nil then
  begin
    P := GetNodeData(aNode);
    Result := TLibraryData(P^);
  end;
end;

procedure TLibraryEditor.DoFreeNode(Node: PVirtualNode);
begin
  GetData(Node).Free;
  inherited DoFreeNode(Node);
end;

procedure TLibraryEditor.DoChange(Node: PVirtualNode);
begin
  inherited DoChange(Node);
  if Node <> nil then
  	if Assigned(fOnNodeSelect) then
    	fOnNodeSelect(Self, GetData(Node));
end;

procedure TLibraryEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  CellText := GetData(Node).Caption;
  inherited DoGetText(Node, Column, TextType, CellText);
end;

function TLibraryEditor.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var Index: integer): TCustomImageList;
begin
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
  Index := GetData(Node).ImageIndex;
end;

function TLibraryEditor.GetLibrary: ILibrary;
begin
  Result := fLibrary;
end;

procedure TLibraryEditor.AfterConstruction;
begin
  inherited AfterConstruction;

  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus,
  	toFullRowSelect, toRightClickSelect];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines] +
    [toShowVertGridLines, toShowHorzGridLines];
  LineStyle := lsSolid;

  DefaultNodeHeight := 24;

  NodeDataSize := SizeOf(TLibraryData);
end;

procedure TLibraryEditor.LoadLibrary;
var
  P: Pointer;
  DeveloperModule, UserModule: string;
  Builder: ILibraryBuilder;
begin

  DeveloperModule := GetSetting.DeveloperLibrary + '\module.jlf';
  if not FileExists(Utf8ToAnsi(DeveloperModule)) then
    raise Exception.Create('Не найден файл содержания библиотеки разработчика');

  UserModule := GetSetting.UserLibrary + '\module.jlf';
  if not FileExists(Utf8ToAnsi(UserModule)) then
    raise Exception.Create('Не найден файл содержания библиотеки пользователя');

  fLibrary := uLibrary.GetLibrary([Utf8ToAnsi(DeveloperModule), Utf8ToAnsi(UserModule)]);


  //Список библиотек
  if fLibrary <> nil then
    for P in fLibrary do
    begin
      Builder := TSublibraryBuilder.Create(fLibrary.ExtractKey(P),
        fLibrary.ExtractData(P));
      Builder.Tree := Self;
      TLibraryDirector.Build(Builder);
    end;

end;


end.


