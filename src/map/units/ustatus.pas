unit uStatus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl;

type
  IStatusItem = interface
  ['{149F06C3-1283-4AB4-9374-4A5E7AFB5143}']

    function GetDescription: string;
    function GetName: string;
    function GetShortDescription: string;
    procedure SetDescription(const aDescription: string);
    procedure SetName(const aName: string);
    procedure SetShortDescription(const aShortDescription: string);

    property Name: string read GetName write SetName;
    property ShortDescription: string read GetShortDescription write SetShortDescription;
    property Description: string read GetDescription write SetDescription;

  end;

  { TStatusItem }

  TStatusItem = class(TInterfacedObject, IStatusItem)
  private
    fName: string;
    fShortDescription: string;
    fDescription: string;
    function GetDescription: string;
    function GetName: string;
    function GetShortDescription: string;
    procedure SetDescription(const aDescription: string);
    procedure SetName(const aName: string);
    procedure SetShortDescription(const aShortDescription: string);
  public
    property Name: string read GetName write SetName;
    property ShortDescription: string read GetShortDescription write SetShortDescription;
    property Description: string read GetDescription write SetDescription;
  end;

  TStatus = specialize TFpgMap<integer, IStatusItem>;


implementation

{ TStatusItem }

function TStatusItem.GetName: string;
begin
  Result := fName;
end;

function TStatusItem.GetDescription: string;
begin
  Result := fDescription;
end;

function TStatusItem.GetShortDescription: string;
begin
  Result := fShortDescription;
end;

procedure TStatusItem.SetDescription(const aDescription: string);
begin
  if fDescription <> aDescription then
     fDescription := aDescription
end;

procedure TStatusItem.SetName(const aName: string);
begin
  if fName <> aName then
     fName := aName
end;

procedure TStatusItem.SetShortDescription(const aShortDescription: string);
begin
  if fShortDescription <> aShortDescription then
     fShortDescription := aShortDescription
end;

end.

