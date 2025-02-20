program QuadtreeVisCulling;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, GLScene_RunTime, Interfaces,
  fQuadtreeVisCulling in 'fQuadtreeVisCulling.pas' {frmQuadtreeVisCulling};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmQuadtreeVisCulling, frmQuadtreeVisCulling);
  Application.Run;
end.
