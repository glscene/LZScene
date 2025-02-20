program FurBall;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, Interfaces,
  fFurBall in 'fFurBall.pas' {frmFurBall};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFurBall, frmFurBall);
  Application.Run;
end.
