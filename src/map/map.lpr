library map;

{$mode objfpc}{$H+}

uses
  ShareMem, Classes, uMapCls, uMap, uStatus, uStatusSaxParser;

function GetMap: IMap; export;
begin
  Result := TMap.Create;
end;

exports
  GetMap;

{$R *.res}

begin
end.

