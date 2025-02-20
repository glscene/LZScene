program GLSceneMolecularViewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GLScene_RunTime, uMainForm, uHelpCommandsForm, uCPKForm, uAtomInfosForm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(THelpCommandsForm, HelpCommandsForm);
  Application.CreateForm(TCPKForm, CPKForm);
  Application.CreateForm(TAtomInfosForm, AtomInfosForm);
  Application.Run;
end.

