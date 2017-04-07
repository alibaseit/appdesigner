program appdesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, zcomponent, Main, ModuleFrame, maindm, editorframe, modeltemplate
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainDM, MainDataModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

