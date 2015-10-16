library setting;

{$mode objfpc}{$H+}

uses
  Sharemem, Classes, usetting, usettingcls, uRegSetting;

var
  S: ISetting;
  RegSetting: ISetting;

  function GetSetting(const aFileName: string = ''): ISetting; export;
  begin
    if S = nil then
      S := TSetting.Create(aFileName);
    Result := S;
  end;

function GetRegSetting: ISetting; export;
begin
  if RegSetting = nil then
    RegSetting := TRegSetting.Create();
  Result := RegSetting;
end;


exports
  GetSetting,
	GetRegSetting;

{$R *.res}

begin
end.

