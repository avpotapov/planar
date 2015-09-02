unit uDetailedProxyForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  uConfiguratorData;

type

  { TDetailedProxyForm }

  TDetailedProxyForm = class
  private
    fParent:  TWinControl;
    fCurrent: TDetailedForm;

  private
    function Find(const aFormClass: TDetailedFormClass): TDetailedForm;
    function Creator(const aFormClass: TDetailedFormClass): TDetailedForm;
  public
    constructor Create(const aParent: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure View(const aContentData: array of TContentData);
    procedure Hide;
  end;

implementation

constructor TDetailedProxyForm.Create(const aParent: TWinControl);
begin
  inherited Create;
  fParent  := aParent;
  fCurrent := nil;
end;

destructor TDetailedProxyForm.Destroy;
begin
  Hide;

  inherited Destroy;
end;

procedure TDetailedProxyForm.View(const aContentData: array of TContentData);
begin
  if Length(aContentData) = 0 then
    Exit;
  if aContentData[0].FormClass = nil then
    Exit;

  Hide;

  try
    // Загрузить форму в соответствии с классовой ссылкой
    if fCurrent = nil then
    begin
      fCurrent := Creator(aContentData[0].FormClass);
      // Показать
      fCurrent.Show;
      Exit;
    end;

    if not (fCurrent is aContentData[0].FormClass) then
    begin
      // Скрыть форму
      fCurrent.Hide;

      // Поиск формы
      fCurrent := Find(aContentData[0].FormClass);

      // Если не найдена, создается новая
      if fCurrent = nil then
        fCurrent := Creator(aContentData[0].FormClass);

      // Показать
      fCurrent.Show;
    end;

  finally
    // Загрузить новые параметры
    fCurrent.Load(aContentData);
  end;
end;

procedure TDetailedProxyForm.Hide;
begin
  if fCurrent <> nil then
     fCurrent.Unload;
end;

function TDetailedProxyForm.Find(const aFormClass: TDetailedFormClass
  ): TDetailedForm;
var
  Index: integer;
begin
  Result := nil;
  // Поиск дочерних элементов управления(форм) в контейнере

  for Index := 0 to fParent.ControlCount - 1 do
    if fParent.Controls[Index] is aFormClass then
    begin
      Result := fParent.Controls[Index] as TDetailedForm;
      Break;
    end;

end;

function TDetailedProxyForm.Creator(const aFormClass: TDetailedFormClass
  ): TDetailedForm;
begin
  Result := aFormClass.Create(Application);
  Result.Parent := fParent;
  Result.Align := alClient;

end;

end.


