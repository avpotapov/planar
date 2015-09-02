unit ubasedescform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, VirtualTrees, Windows,
  uLibraryData,
  uLibrary;

type

  { TBaseDescForm }

  TBaseDescForm = class(TEditingForm)
    FileNameEdit: TFileNameEdit;
    Image: TImage;
    Desc: TMemo;
    Panel: TPanel;
    Splitter: TSplitter;
    procedure DescEditingDone(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);

  private
    fBaseDescData: TBaseDescData;

    function GetImageDir: string;
    procedure LoadImage(const aFileName: string);
    procedure SaveImage(const aFileName: string);
  public
    procedure Load(const aLibraryData: TLibraryData); override;
    procedure Unload; override;
  end;



implementation

uses
  uSetting;

{$R *.lfm}

{ TBaseDescForm }

type

  { TBaseVirtualTreeHelper }

  TBaseVirtualTreeHelper = class Helper for TBaseVirtualTree
  private
    function GetData(const aNode: PVirtualNode): TLibraryData;
  end;


function TBaseVirtualTreeHelper.GetData(const aNode: PVirtualNode): TLibraryData;
var
  P: Pointer;
begin
  Result := nil;
  if aNode = nil then
    Exit;
  P := GetNodeData(aNode);
  Result := TLibraryData(P^);
end;

procedure TBaseDescForm.FileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
	SaveImage(Value);
  fBaseDescData.BaseDesc.Image := ExtractFileName(Value);
  LoadImage(GetImageDir + PathDelim + fBaseDescData.BaseDesc.Image);
end;

procedure TBaseDescForm.DescEditingDone(Sender: TObject);
begin
  fBaseDescData.BaseDesc.Description.Text := Desc.Lines.Text;
end;

function TBaseDescForm.GetImageDir: string;
var
  LibraryData: TLibraryData;
begin
  Result := '';
  LibraryData := fBaseDescData.Tree.GetData(fBaseDescData.OwnerNode^.Parent^.Parent);
  if LibraryData is TSublibraryData then
     case TSublibraryData(LibraryData).TypeSublibrary of
     	slDeveloper: Result := GetSetting.DeveloperImage;
     	slUser: Result := GetSetting.UserImage;
     end;
end;


procedure TBaseDescForm.LoadImage(const aFileName: string);
begin
  if fBaseDescData.BaseDesc.Image <> '' then
   try
     Image.Picture.LoadFromFile(aFileName);
   except
     on E: Exception do
       MessageBox(Handle, PChar('Ошибка загрузки изображения'),
         PChar(Utf8ToAnsi('Ошибка загрузки')), MB_ICONERROR + MB_OK);
   end;
end;

procedure TBaseDescForm.SaveImage(const aFileName: string);
var
  FileName: string;
  NewFileName: string;
begin
  if aFileName = '' then
     Exit;
   try
     FileName := ExtractFileName(aFileName);
     NewFileName := GetImageDir + PathDelim + FileName;
     if not FileExists(Utf8ToAnsi(NewFileName)) then
       FileUtil.CopyFile(Utf8ToAnsi(aFileName), Utf8ToAnsi(NewFileName));
   except
     MessageBox(Handle, PChar(Utf8ToAnsi('Ошибка сохранения файла изображения')),
           PChar(Utf8ToAnsi('Ошибка сохранения')), MB_ICONERROR + MB_OK);
   end;

end;

procedure TBaseDescForm.Load(const aLibraryData: TLibraryData);
begin
  if aLibraryData is TBaseDescData then
    fBaseDescData := TBaseDescData(aLibraryData);

   if fBaseDescData.BaseDesc.Image = '' then
    Exit;

  Desc.Lines.Text := fBaseDescData.BaseDesc.Description.Text;

  FileNameEdit.InitialDir := GetImageDir;
  FileNameEdit.Text := fBaseDescData.BaseDesc.Image;
  LoadImage(FileNameEdit.InitialDir + PathDelim + fBaseDescData.BaseDesc.Image);

end;

procedure TBaseDescForm.Unload;
begin
  Desc.Lines.Clear;
  FileNameEdit.Text := '';
  Image.Picture.Clear;
end;

end.
