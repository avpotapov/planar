library setting;

{$mode objfpc}{$H+}

uses
  Sharemem, Classes, usetting, usettingcls;

var
  S: ISetting;

  function GetSetting(const aFileName: string = ''): ISetting; export;
  begin
    if S = nil then
      S := TSetting.Create(aFileName);
    Result := S;
  end;

exports
  GetSetting;

{$R *.res}

begin
end.

