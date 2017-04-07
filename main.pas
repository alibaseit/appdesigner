unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, ActnList, Buttons, DBGrids, DbCtrls, StdCtrls,
  sqlite3conn, sqldb, ModuleFrame, maindm, editorframe;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddAttributeAction: TAction;
    AddModelAction: TAction;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    QuitApplicationAction: TAction;
    SaveProjectAction: TAction;
    OpenProjectAction: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    TabSheet1: TTabSheet;
    ModelTab: TTabSheet;
    EditorTab: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FModuleFrm: TModuleFrame;
    FEditorFrm: TEditorFrm;
    procedure CreateEditorTab;
    procedure CreateModelPanel;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  OpenMenuItem: TMenuItem;
begin
  OpenMenuItem := TMenuItem.Create(self);
  OpenMenuItem.Action := OpenProjectAction;
  MainMenu.Items[0].Add(OpenMenuItem);
  MainDataModule.ActivateConnectionAndAllQueries;
  CreateEditorTab;
  CreateModelPanel;
  FModuleFrm.SetEditorFrame(FEditorFrm);
end;

procedure TMainForm.CreateModelPanel;
begin
  FModuleFrm := TModuleFrame.Create(ModelTab);
  FModuleFrm.Parent := ModelTab;
  FModuleFrm.Align := alClient;
end;

procedure TMainForm.CreateEditorTab;
begin
  FEditorFrm := TEditorFrm.Create(EditorTab);
  FEditorFrm.Parent := EditorTab;
  FEditorFrm.Align := alClient;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MainDataModule.CloseConnection;
end;

end.

