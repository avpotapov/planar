unit uEditingFormPool;

(*
  Является контейнером форм редактирования объектов. Если форма пока не создана,
  то после ее создания и размещения в контейнере, она выводится на экран.
  Если форма уже создана, то отображается на экране.
*)


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  uLibraryData;

type

  { TEditingFormPool }

  TEditingFormPool = class
  private
    fParent:  TWinControl;
    fCurrent: TEditingForm;

  private
    function Find(const aFormClass: TEditingFormClass): TEditingForm;
    function Creator(const aFormClass: TEditingFormClass): TEditingForm;
  public
    constructor Create(const aParent: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure View(const aLibraryData: TLibraryData);
    procedure Hide;
  end;

implementation

constructor TEditingFormPool.Create(const aParent: TWinControl);
begin
  inherited Create;
  fParent  := aParent;
  fCurrent := nil;
end;

destructor TEditingFormPool.Destroy;
begin
  Hide;
  inherited Destroy;
end;

procedure TEditingFormPool.View(const aLibraryData: TLibraryData);
begin
  if aLibraryData = nil then
    Exit;
  if aLibraryData.FormClass = nil then
    Exit;

  Hide;

  try
    // Загрузить форму в соответствии с классовой ссылкой
    if fCurrent = nil then
    begin
      fCurrent := Creator(aLibraryData.FormClass);
      // Показать
      fCurrent.Show;
      Exit;
    end;

    if not (fCurrent is aLibraryData.FormClass) then
    begin
      // Скрыть форму
      fCurrent.Hide;

      // Поиск формы
      fCurrent := Find(aLibraryData.FormClass);

      // Если не найдена, создается новая
      if fCurrent = nil then
        fCurrent := Creator(aLibraryData.FormClass);

      // Показать
      fCurrent.Show;
    end;

  finally
    // Загрузить новые параметры
    fCurrent.Load(aLibraryData);
  end;
end;

procedure TEditingFormPool.Hide;
begin
  if fCurrent <> nil then
     fCurrent.Unload;
end;

function TEditingFormPool.Find(const aFormClass: TEditingFormClass
  ): TEditingForm;
var
  Index: integer;
begin
  Result := nil;
  // Поиск дочерних элементов управления(форм) в контейнере

  for Index := 0 to fParent.ControlCount - 1 do
    if fParent.Controls[Index] is aFormClass then
    begin
      Result := fParent.Controls[Index] as TEditingForm;
      Break;
    end;

end;

function TEditingFormPool.Creator(const aFormClass: TEditingFormClass
  ): TEditingForm;
begin
  Result := aFormClass.Create(Application);
  Result.Parent := fParent;
  Result.Align := alClient;
end;

end.

