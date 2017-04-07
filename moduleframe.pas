unit ModuleFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, DBGrids,
  DbCtrls, Menus, sqldb, sqlite3conn, maindm, editorframe;

type

  { TModuleFrame }

  TModuleFrame = class(TFrame)
    AddAtributePopupMenu: TMenuItem;
    AttributeGrid: TDBGrid;
    CommitBtn: TButton;
    DBCheckBox1: TDBCheckBox;
    GenerateModelCodeBtn: TButton;
    TypeCombo: TDBComboBox;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    NameEdt: TDBEdit;
    DescriptionEdt: TDBEdit;
    FieldNameEdt: TDBEdit;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ModelAddMenu: TMenuItem;
    ModelGrid: TDBGrid;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure CommitBtnClick(Sender: TObject);
    procedure GenerateModelCodeBtnClick(Sender: TObject);
  private
    FEditorFrm: TEditorFrm;
  public
    procedure SetEditorFrame(AEditorFrm: TEditorFrm);
  end;

implementation

{$R *.lfm}

{ TModuleFrame }

procedure TModuleFrame.CommitBtnClick(Sender: TObject);
begin
  MainDataModule.SaveAllChanges;
end;

procedure TModuleFrame.GenerateModelCodeBtnClick(Sender: TObject);
begin
  FEditorFrm.GenerateModelScript(MainDataModule.ModelQuery.FieldByName('id').AsInteger);
end;

procedure TModuleFrame.SetEditorFrame(AEditorFrm: TEditorFrm);
begin
   FEditorFrm := AEditorFrm;
end;

end.

