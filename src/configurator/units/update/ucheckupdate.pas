unit uCheckUpdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, uSetting, fgl, Dom, XmlRead;

const
  URL = 'http://planar-smt.ru/updates/jlconfigurator/update.dat';
  SAVED_FILE_NAME = 'update.dat';

type

  { TUpdateInfo }

  TUpdateInfo = class
  private
    FDescription: TStringList;
    FFileLink: string;
    FVersion: string;
    function GetDescription: TStringList;
  public
    destructor Destroy; override;
    property Description: TStringList read GetDescription;
    property FileLink: string read FFileLink write FFileLink;
    property Version: string read FVersion write FVersion;
  end;


  { TUpdate }

  TUpdateObject = specialize TFpgObjectList<TUpdateInfo>;

  TUpdate = class
  private
    FUpdateSoft: TUpdateObject;
    FUpdateLib: TUpdateObject;
    function GetUpdateLib: TUpdateObject;
    function GetUpdateSoft: TUpdateObject;
  public
    destructor Destroy; override;
    procedure Parse(const AFileName: string = '');
    class function GetUpdateFile: boolean;
    property UpdateSoft: TUpdateObject read GetUpdateSoft;
    property UpdateLib: TUpdateObject read GetUpdateLib;
  end;




implementation

{ TUpdateInfo }

function TUpdateInfo.GetDescription: TStringList;
begin
  if FDescription = nil then
    FDescription := TStringList.Create;
  Result := FDescription;
end;

destructor TUpdateInfo.Destroy;
begin
  FreeAndNil(FDEscription);
  inherited Destroy;
end;


{ TUpdate }

class function TUpdate.GetUpdateFile: boolean;
var
  FileStream: TFileStream;
  F: TExtFile;
  Path: string;
  FileName: string;
begin
  Result := False;
  // Создать файл
  Path := uSetting.GetSetting.UserData;
  if Path <> '' then
    FileName := IncludeTrailingPathDelimiter(Path) + SAVED_FILE_NAME
  else
    FileName := SAVED_FILE_NAME;

  //AssignFile(F, FileName);
  //ReWrite(F);
  //CloseFile(F);

  // Создаём файловый поток

  FileStream := TFileStream.Create(FileName, fmCreate);

  try
    with THTTPSend.Create do
    begin
      if HTTPMethod('GET', URL) then
        try
          Document.SaveToStream(FileStream);
          Result := True;
        except

        end;
      Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function TUpdate.GetUpdateSoft: TUpdateObject;
begin
  if FUpdateSoft = nil then
    FUpdateSoft := TUpdateObject.Create;
  Result := FUpdateSoft;
end;

function TUpdate.GetUpdateLib: TUpdateObject;
begin
  if FUpdateLib = nil then
    FUpdateLib := TUpdateObject.Create;
  Result := FUpdateLib;

end;

destructor TUpdate.Destroy;
begin
  FreeAndNil(FUpdateSoft);
  FreeAndNil(FUpdateLib);
  inherited Destroy;
end;

procedure TUpdate.Parse(const AFileName: string);

  procedure InternalParse(const ADoc: TXMLDocument; const ATagName: string;
  const AUpdate: TUpdateObject);
  var
    Node: TDOMNode;
    UpdateInfo: TUpdateInfo;
    I: integer;
  begin
    Node := ADoc.DocumentElement.FindNode(ATagName);
    Node := Node.FirstChild;
    while Assigned(Node) do
    begin
      UpdateInfo := TUpdateInfo.Create;
      // Version
      UpdateInfo.Version := Node.Attributes.Item[0].NodeValue;
      // Description
      with Node.ChildNodes.Item[0].ChildNodes do
        for I := 0 to (Count - 1) do
          UpdateInfo.Description.Add(AnsiToUtf8(Item[I].FirstChild.NodeValue));
      // FileLink
      UpdateInfo.FileLink := Node.ChildNodes.Item[1].FirstChild.NodeValue;
      AUpdate.Add(UpdateInfo);
      Node := Node.NextSibling;
    end;
  end;

var
  Doc: TXMLDocument;
  FileName: string;
  Path: string;
begin
  FileName := AFileName;
  if FileName = '' then
  begin
    Path := uSetting.GetSetting.UserData;
    FileName := IncludeTrailingPathDelimiter(Path) + SAVED_FILE_NAME;
  end;

  if not FileExists(FileName) then
    exit;
  // Парсинг XML
  ReadXMLFile(Doc, FileName);
  try
    // Soft
    InternalParse(Doc, 'soft', UpdateSoft);
    // Lib
    InternalParse(Doc, 'lib', UpdateLib);
  finally
    Doc.Free;
  end;

end;

end.
