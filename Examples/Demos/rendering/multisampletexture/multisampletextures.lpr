program MultiSampleTextures;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, GLScene_RunTime, Interfaces,
  uMain in 'uMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLDemoForm, GLDemoForm);
  Application.Run;
end.
