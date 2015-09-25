library lib;

{$mode objfpc}{$H+}
uses
  ShareMem,
  SysUtils,
  Classes,
  uBase,
  ulibrary,
  uLibraryCls,
  uSaxBase,
  uLibrarySaxParser,
  uLibraryBuilder,
  uModuleBuilder, uLibrarySerializer;

var
  L: ILibrary;



  function GetLibrary(const aFileNames: array of string): ILibrary; export;
  begin
    if L = nil then
      with TLibraryFactory.Create do
      try
         L := GetLibrary(aFileNames);
      finally
        Free;
      end;
    Result := L;
  end;

  procedure CloseLibrary; export;
  begin
    L := nil;
  end;

  function GetNewGuid: String; export;
  var
    Guid:      TGuid;
    StartChar: Integer;
    EndChar:   Integer;
    Count:     Integer;
  begin
    CreateGuid(Guid);
    Result := GuidToString(Guid);

    StartChar := Pos('{', Result) + 1;
    EndChar   := Pos('}', Result) - 1;
    Count     := EndChar - StartChar + 1;

    Result := Copy(Result, StartChar, Count);
  end;

  procedure SaveLibrary(const aLibrary: ILibrary); export;
  var
    Serializer: TLibrarySerializer;
  begin
    Serializer := TLibrarySerializer.Create;
    try
       Serializer.Serialize(aLibrary);
    finally
      Serializer.Free;
    end;
  end;



exports
  GetLibrary,
	SaveLibrary,
	CloseLibrary,
  GetNewGuid;


{$R *.res}

begin
end.
