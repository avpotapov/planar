unit uStatusSaxParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows,
  SAX, SAX_XML,
  uStatus;

type
  {$REGION SaxHandler}

  { TSaxHandler }

  TSaxHandler = class
  protected
    fStatus: TStatus;
    fCurrentTagName: String;

    fName:  String;
    fValue: Integer;
    fShortDescription: String;
    fDescription: String;

  public
    constructor Create(const aStatus: TStatus); reintroduce;

  public
    procedure OnStartElement(Sender: TObject;
      const {%H-}aNamespaceURI, aLocalName, {%H-}aQName: SAXString;
    {%H-}aAtts: TSAXAttributes); virtual;

    procedure OnEndElement(Sender: TObject; const {%H-}aNamespaceURI,
    {%H-}aLocalName, {%H-}aQName: SAXString); virtual;

    procedure OnCharacters(Sender: TObject; const aCh: PSAXChar;
    {%H-}aStart, aLength: Integer); virtual;

    procedure OnError(Sender: TObject; aException: ESAXParseException); virtual;

    procedure OnFatalError(Sender: TObject; aException: ESAXParseException); virtual;

  public

  end;

{$ENDREGION SaxHandler}

{$REGION Factory}

  { TStatusFactory }

  TStatusFactory = class
  private
    fSaxReader:  TSaxReader;
    fSaxHandler: TSaxHandler;

    fStatus: TStatus;
  private
    procedure ConstructSaxHandler;
    procedure ConstructSaxReader;
    procedure AssignHandler;
  public
    constructor Create(const aStatus: TStatus); reintroduce;
  public
    procedure Parse(aFileName: String = '');
  end;

  {$ENDREGION Factory}

implementation

{ TSaxHandler }

constructor TSaxHandler.Create(const aStatus: TStatus);
begin
  inherited Create;
  fStatus := aStatus;
end;


procedure TSaxHandler.OnStartElement(Sender: TObject;
  const aNamespaceURI, aLocalName, aQName: SAXString; aAtts: TSAXAttributes);
begin
  fCurrentTagName := aLocalName;
end;

procedure TSaxHandler.OnEndElement(Sender: TObject;
  const aNamespaceURI, aLocalName, aQName: SAXString);
var
  I: Integer;
begin
  if aLocalName = 'status_item' then
  begin
    I := fStatus.Add(fValue);
    if I > -1 then
    begin
      fStatus.Data[I]      := TStatusItem.Create as IStatusItem;
      fStatus.Data[I].Name := fName;
      fStatus.Data[I].Description := fDescription;
      fStatus.Data[I].ShortDescription := fShortDescription;
    end;
  end;
end;

procedure TSaxHandler.OnCharacters(Sender: TObject; const aCh: PSAXChar;
  aStart, aLength: Integer);
var
  S: SaxString;
begin
  // Сохранение массива символов Ansi в строку Utf8
  SetLength(S, aLength);
  Move(aCh^, S[1], aLength * SizeOf(SaxChar));

  if fCurrentTagName = 'name' then
    fName := S;

  if fCurrentTagName = 'value' then
    fValue := StrToIntDef(S, 0);

  if fCurrentTagName = 'short_description' then
    fShortDescription := S;

  if fCurrentTagName = 'description' then
    fDescription := S;

end;

procedure TSaxHandler.OnError(Sender: TObject; aException: ESAXParseException);
begin
  MessageBox(0, PChar(aException.Message),
    PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
    MB_ICONERROR + MB_OK);
end;

procedure TSaxHandler.OnFatalError(Sender: TObject; aException: ESAXParseException);
begin
  MessageBox(0, PChar(aException.Message),
    PChar(Utf8ToAnsi('Фатальная ошибка парсинга XML-файла')),
    MB_ICONSTOP + MB_OK);
end;

{$REGION Factory}

constructor TStatusFactory.Create(const aStatus: TStatus);
begin
  inherited Create;
  fStatus := aStatus;
end;

procedure TStatusFactory.ConstructSaxHandler;
begin
  fSaxHandler := TSaxHandler.Create(fStatus);
end;

procedure TStatusFactory.ConstructSaxReader;
begin
  fSaxReader := TSaxXmlReader.Create;
end;

procedure TStatusFactory.AssignHandler;
begin
  {$IFDEF DEBUG}
  Assert(fSaxReader <> nil);
  Assert(fSaxHandler <> nil);
  {$ENDIF}
  // Назначение обработчиков парсера
  fSaxReader.OnCharacters := @fSaxHandler.OnCharacters;
  fSaxReader.OnStartElement := @fSaxHandler.OnStartElement;
  fSaxReader.OnEndElement := @fSaxHandler.OnEndElement;
  fSaxReader.OnError      := @fSaxHandler.OnError;
  fSaxReader.OnFatalError := @fSaxHandler.OnFatalError;
end;



procedure TStatusFactory.Parse(aFileName: String);
var
  Stream: TStream;
begin
  if aFileName = '' then
    aFileName := 'status.xml';



  Stream := TFileStream.Create(aFileName, fmOpenRead);

  ConstructSaxHandler;
  ConstructSaxReader;
  AssignHandler;

  try

    try
      fSaxReader.ParseStream(Stream);
    except
      on E: ESAXParseException do
      begin
        MessageBox(0, PChar(Utf8ToAnsi(E.Message)),
          PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
          MB_ICONERROR + MB_OK);
      end;
      else
        MessageBox(0, PChar(Utf8ToAnsi('Неизвестная ошибка')),
          PChar(Utf8ToAnsi('Ошибка парсинга XML-файла')),
          MB_ICONERROR + MB_OK);
    end;

  finally
    FreeAndNil(fSaxHandler);
    FreeAndNil(fSaxReader);
    FreeAndNil(Stream);
  end;
end;

{$ENDREGION Factory}
end.
