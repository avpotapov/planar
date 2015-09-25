unit uMenuHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus;
type

  { TPopupMenuHelper }

  TPopupMenuHelper = class helper for TPopupMenu
  public
    procedure AddMenuItem(const aCaption: string; const aClick: TNotifyEvent = nil);
    procedure AddMenuItem(const aParent: TMenuItem; const aCaption: string;
      const aImageIndex: Integer; const aClick: TNotifyEvent = nil);
    function AddSubMenu(const aCaption: string): TMenuItem;
  end;


implementation

{ TPopupMenuHelper }

procedure TPopupMenuHelper.AddMenuItem(const aCaption: string;
  const aClick: TNotifyEvent);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := aCaption;
  MenuItem.OnClick := aClick;
  Items.Add(MenuItem);
end;

procedure TPopupMenuHelper.AddMenuItem(const aParent: TMenuItem; const aCaption: string;
  const aImageIndex: Integer; const aClick: TNotifyEvent);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := aCaption;
  MenuItem.ImageIndex := aImageIndex;
  MenuItem.OnClick := aClick;
  aParent.Add(MenuItem);

end;

function TPopupMenuHelper.AddSubMenu(const aCaption: string): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := aCaption;
  Items.Add(Result);
end;

end.

