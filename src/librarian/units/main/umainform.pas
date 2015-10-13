unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, VirtualTrees, Windows,
	uLibrary,
  uLibraryData,
  uLibraryEditor,
  uEditingFormPool;

type

  { TMainForm }

  TMainForm = class (TForm)
    ApplicationMenuItem: TMenuItem;
    LibraryPanel: TPanel;
    LibraryImageList: TImageList;
    LibraryTreePopupMenu: TPopupMenu;
    FormPanel:    TPanel;
    ExitMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    SaveLibraryMenuItem: TMenuItem;
    Separator: TMenuItem;
    PopupImageList: TImageList;
    MainMenu:     TMainMenu;
    MainSplitter: TSplitter;
    SettingsMenuItem: TMenuItem;
    StatusBar:    TStatusBar;


    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure SaveLibraryMenuItemClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);

  private
    fLibraryEditor: TLibraryEditor;
    fEditingFormPool: TEditingFormPool;

  private
    procedure NodeSelect(Sender: TObject; const aLibraryData: TLibraryData);
  end;

var
  MainForm: TMainForm;

implementation

uses
  uSplashForm,
  uSettingForm, uAboutForm;

{$R *.lfm}


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  SplashForm: TSplashForm;
begin
  SplashForm := TSplashForm.Create('librarian.png');
    try
      SplashForm.Show;
      SplashForm.Repaint;

      fEditingFormPool := TEditingFormPool.Create(FormPanel);

      // Создать библиотеку в дереве
      fLibraryEditor := TLibraryEditor.Create(Self);

      try
        fLibraryEditor.Parent := LibraryPanel;
        fLibraryEditor.Align := alClient;
        fLibraryEditor.Images := LibraryImageList;
        fLibraryEditor.OnNodeSelect := @NodeSelect;
        fLibraryEditor.LoadLibrary;
        sleep(1000);
      except
        on E: Exception do
        begin
          MessageBox(Handle, PChar(Utf8ToAnsi(E.Message)),
            PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
          Application.Terminate;
        end
        else
          MessageBox(Handle, PChar(Utf8ToAnsi('Ошибка чтения библиотеки')),
            PChar(Utf8ToAnsi('Ошибка')), MB_ICONERROR + MB_OK);
      end;

    finally
      SplashForm.Release;
    end;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
begin
  with TAboutForm.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.SaveLibraryMenuItemClick(Sender: TObject);
begin
  SaveLibrary(fLibraryEditor.GetLibrary);
  StatusBar.Panels[0].Text := 'Изменения сохранены';
end;

procedure TMainForm.SettingsMenuItemClick(Sender: TObject);
begin
  // Диалог настройки программы
  TSettingForm.Create(Application).ShowModal;
end;

procedure TMainForm.NodeSelect(Sender: TObject;
  const aLibraryData: TLibraryData);
begin
  fEditingFormPool.View(aLibraryData);
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fEditingFormPool.Free;
end;



end.




