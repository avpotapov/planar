unit uHoldingsSaxParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, SAX, SAX_XML,
  uSaxBase, uHoldingsBuilder, uHoldings;

type

   { THoldingsSaxFactory }

  THoldingsSaxFactory = class
  private
    fSaxReader:  TSaxReader;
    fSaxHandler: TSaxHandler;

  private
    procedure ConstructSaxHandler;
    procedure ConstructSaxReader;
    procedure AssignHandler;
  public
    function GetHoldings(const aFileName: string): IHoldings;
  end;


  { TSaxHoldingsHandler }

  TSaxHoldingsHandler = class(TSaxHandler)
  private
    fHoldings: IHoldings;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    property Holdings: IHoldings read fHoldings;
  end;


implementation

uses uHoldingsCls;

{ THoldingsSaxFactory }

procedure THoldingsSaxFactory.ConstructSaxHandler;
begin
  fSaxHandler := TSaxHoldingsHandler.Create;
end;

procedure THoldingsSaxFactory.ConstructSaxReader;
begin
  fSaxReader := TSaxXmlReader.Create;
end;

procedure THoldingsSaxFactory.AssignHandler;
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

function THoldingsSaxFactory.GetHoldings(const aFileName: string): IHoldings;
var
  Stream: TStream;
begin
  Result := nil;
  Stream := TFileStream.Create(utf8ToAnsi(aFileName), fmOpenRead);

  ConstructSaxHandler;
  ConstructSaxReader;
  AssignHandler;

  try

    try
      fSaxReader.ParseStream(Stream);
      Result := (fSaxHandler as TSaxHoldingsHandler).Holdings;
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

{ TSaxHoldingsHandler }

procedure TSaxHoldingsHandler.AfterConstruction;
var
  HoldingsBuilder: IBuilder;
  HoldingBuilder:  IBuilder;
begin
  {$IFDEF DEBUG}
  Assert(fIntfStack.IsEmpty);
  Assert(fTagStack.IsEmpty);
  {$ENDIF}

  inherited AfterConstruction;

  {$IFDEF DEBUG}
  Assert(fTagStack.Top = tnEmpty);
  {$ENDIF}

  fHoldings := THoldings.Create;
  fIntfStack.Push(fHoldings);

  // Строители
  HoldingsBuilder := THoldingsBuilder.Create(Self);
  HoldingBuilder  := THoldingBuilder.Create(Self);

  // Таблица переходов
  fBuilderTransitionTable[tnEmpty, tnHoldings]    := HoldingsBuilder;
  fBuilderTransitionTable[tnHoldings, tnHoldings] := HoldingsBuilder;

  fBuilderTransitionTable[tnHoldings, tnHolding] := HoldingBuilder;
  fBuilderTransitionTable[tnHolding, tnHolding]  := HoldingBuilder;

end;

procedure TSaxHoldingsHandler.BeforeDestruction;
var
  Intf: IInterface;
begin
  // Удалить последний интерфейс со стека
  fIntfStack.Pop(Intf);

  {$IFDEF DEBUG}
  Assert(fTagStack.Top = tnEmpty);
  {$ENDIF}

  inherited BeforeDestruction;

  {$IFDEF DEBUG}
  Assert(fIntfStack.IsEmpty);
  {$ENDIF}
end;

end.

