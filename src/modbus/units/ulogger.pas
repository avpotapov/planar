unit uLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ubase, uSetting;

type

  { TCustomLogger }

  { TLogger }

  TLogger = class
  private
    FFileHandle : TextFile;
    fLoggerName : string;
    fApplicationPath : string;

  public
    constructor Create(const aLoggerName: string = '');
    destructor Destroy; override;
    function GetApplicationName: string;
    function GetApplicationPath: string;
    procedure LogError( ErrorMessage : string; Location : string );
    procedure LogWarning( WarningMessage : string; Location : string );
    procedure LogStatus( StatusMessage : string; Location : string );
  published
    property ApplicationName : string read GetApplicationName;
    property ApplicationPath : string read GetApplicationPath;
  end;

  TTsLoggerSpec = specialize TThreadSafeWrapper<TLogger>;

  { TTsLogger }
  {
    Потокобезопасный логгер на основе паттерна Singleton
  }
  TTsLogger = class(TTsLoggerSpec)
  private
   class var fInstance: TTsLogger;
    {%H-}constructor Create;
     {%H-}destructor Destroy; reintroduce;
  public
    class function GetInstance: TTsLogger;
    class procedure FreeInstances; reintroduce;
  end;

implementation

{ TLogger }

constructor TLogger.Create(const aLoggerName: string);
var
  FileName : string;
  Path: string;
begin

  if aLoggerName = '' then
    fLoggerName := ExtractFileName( ParamStr(0) )
  else
    fLoggerName := aLoggerName;

  Path := uSetting.GetSetting.UserData;
  if Path <> '' then
  	fApplicationPath := ExtractFilePath( Path )
  else
    fApplicationPath := ExtractFilePath( ParamStr(0) ) ;

  FileName := fApplicationPath + ChangeFileExt( fLoggerName, '.log' );

  if FileExists( FileName ) then
    DeleteFile(FileName);
  AssignFile( fFileHandle, FileName );
  ReWrite( fFileHandle );
end;

destructor TLogger.Destroy;
begin
  CloseFile( FFileHandle );
  inherited;
end;

function TLogger.GetApplicationName: string;
begin
  result := fLoggerName;
end;

function TLogger.GetApplicationPath: string;
begin
  result := fApplicationPath;
end;

procedure TLogger.LogError(ErrorMessage: string; Location: string);
var
  S : string;
begin
  S := '*** ERROR *** : @ ' + TimeToStr(Time) + ' MSG : ' + ErrorMessage + ' IN : ' + Location + #13#10;
  WriteLn( FFileHandle,  S );
  Flush( FFileHandle );
end;

procedure TLogger.LogStatus(StatusMessage: string; Location: string);
var
  S : string;
begin
  S := 'STATUS INFO : @ ' + TimeToStr(Time) + ' MSG : ' + StatusMessage + ' IN : ' + Location + #13#10;
  WriteLn( FFileHandle,  S );
  Flush( FFileHandle );
end;

procedure TLogger.LogWarning(WarningMessage: string; Location: string);
var
  S : string;
begin
  S := '=== WARNING === : @ ' + TimeToStr(Time) + ' MSG : ' + WarningMessage + ' IN : ' + Location + #13#10;
  WriteLn( FFileHandle,  S );
  Flush( FFileHandle );
end;

{ TTsLogger }

constructor TTsLogger.Create;
begin
  inherited Create;
  fT := TLogger.Create('modbus');
  fT.LogStatus( 'Starting Application', 'Initialization' );
end;

destructor TTsLogger.Destroy;
begin
  inherited Destroy;
  fT.LogStatus( 'Terminating Application', 'Finalization' );
  FreeAndNil(fT);
end;


class function TTsLogger.GetInstance: TTsLogger;
begin
  if fInstance = nil then
    fInstance := TTsLogger.Create;
  Result := fInstance;
end;

class procedure TTsLogger.FreeInstances;
begin
  if fInstance <> nil then
    fInstance.Destroy;
end;

end.

