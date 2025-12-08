program Shadows;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, GLScene_RunTime, Interfaces,
  Main in 'Main.pas' {MainFm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFm, MainFm);
  Application.Run;
end.
