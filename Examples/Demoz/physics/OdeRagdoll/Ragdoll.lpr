program Ragdoll;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, Interfaces,
  fRagdoll in 'fRagdoll.pas'{frmRagdoll};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmRagdoll, frmRagdoll);
  Application.Run;
end.
