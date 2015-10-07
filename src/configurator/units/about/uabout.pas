unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileInfo;

var
  ConfiguratorVersion: TFileVersionInfo;
  LibrarianVersion: TFileVersionInfo;

implementation

initialization
  ConfiguratorVersion := TFileVersionInfo.Create(nil);
  ConfiguratorVersion.FileName := AnsiToUtf8(ParamStr(0));

  LibrarianVersion := TFileVersionInfo.Create(nil);
  LibrarianVersion.FileName := AnsiToUtf8(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+ 'librarian.exe');

finalization
  ConfiguratorVersion.Free;
  LibrarianVersion.Free;
end.
