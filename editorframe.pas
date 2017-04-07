unit editorframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterPas, SynHighlighterAny, SynEdit,
  SynHighlighterJava, Forms, Controls, maindm, sqldb, modeltemplate;

type

  { TEditorFrm }

  TEditorFrm = class(TFrame)
    Editor: TSynEdit;
    SynJavaSyn1: TSynJavaSyn;
  private
  public
    procedure GenerateModelScript(AModelId: Integer);
  end;



implementation

{$R *.lfm}

{ TEditorFrm }

procedure TEditorFrm.GenerateModelScript(AModelId: Integer);
begin
  Editor.Text := GenerateModelSource(AModelId);
end;


end.

