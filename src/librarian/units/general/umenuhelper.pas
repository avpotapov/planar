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

end.

