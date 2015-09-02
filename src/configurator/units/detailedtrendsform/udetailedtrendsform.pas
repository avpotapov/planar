unit udetailedtrendsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uConfiguratorData;

type

  { TDetailedTrendsForm }

  TDetailedTrendsForm = class(TDetailedForm)
    Label1: TLabel;
  private
    fModbusData:     TModbusData;
    fDeviceData:     TDeviceData;
    fTrendsData: TTrendsData;
  public
    procedure Load(const aContentData: array of TContentData); override;
    procedure Unload; override;
  end;

var
  DetailedTrendsForm: TDetailedTrendsForm;

implementation

{$R *.lfm}

{ TDetailedTrendsForm }

procedure TDetailedTrendsForm.Load(const aContentData: array of TContentData);
var
  I: Integer;
begin
  {$IFDEF DEBUG}
  Assert(Length(aContentData) = 3);
  {$ENDIF}

  // Инициализация
  for I := Low(aContentData) to High(aContentData) do
  begin
    if aContentData[I] is TModbusData then
      fModbusData := aContentData[I] as TModbusData;
    if aContentData[I] is TDeviceData then
      fDeviceData := aContentData[I] as TDeviceData;
    if aContentData[I] is TTrendsData then
      fTrendsData := aContentData[I] as TTrendsData;
  end;
end;

procedure TDetailedTrendsForm.Unload;
begin
  //
end;

end.

