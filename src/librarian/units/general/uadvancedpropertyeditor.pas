unit uAdvancedPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Menus,
  uPropertyEditorNew,
  uVtBaseEditor;

type

  { TAdvancedPropertyEditor }

  TAdvancedPropertyEditor = class(TPropertyEditor)
  protected
    procedure ConstructPopupMenu; virtual;

    procedure AddNewItem(Sender: TObject); virtual; abstract;
    procedure CloneItem(Sender: TObject);  virtual; abstract;
    procedure DeleteItem(Sender: TObject); virtual; abstract;
    procedure CollapseAll(Sender: TObject); virtual;
    procedure ExpandAll(Sender: TObject);  virtual;

  public
    procedure AfterConstruction; override;
  end;

implementation
uses
  {%H-}uMenuHelper;

{ TAdvancedPropertyEditor }

procedure TAdvancedPropertyEditor.ConstructPopupMenu;
var
  Menu: TPopupMenu;
begin
  Menu := TPopupMenu.Create(Self);
  Menu.Parent := Self;
  PopupMenu := Menu;

  Menu.AddMenuItem('Добавить...', @AddNewItem);
  Menu.AddMenuItem('Клонировать...', @CloneItem);
  Menu.AddMenuItem('-');
  Menu.AddMenuItem('Удалить...', @DeleteItem);
  Menu.AddMenuItem('-');
  Menu.AddMenuItem('Развернуть все', @ExpandAll);
  Menu.AddMenuItem('Свернуть все все', @CollapseAll);

end;

procedure TAdvancedPropertyEditor.CollapseAll(Sender: TObject);
begin
  FullCollapse();
end;

procedure TAdvancedPropertyEditor.ExpandAll(Sender: TObject);
begin
  FullExpand();
end;

procedure TAdvancedPropertyEditor.AfterConstruction;
begin
  inherited AfterConstruction;
  ConstructPopupMenu;
end;

end.

