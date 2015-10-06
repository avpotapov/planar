unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileInfo;

var
  Version: TFileVersionInfo;

implementation

initialization
  Version := TFileVersionInfo.Create(nil);
  Version.FileName := AnsiToUtf8(ParamStr(0));
finalization
  Version.Free;
end.
