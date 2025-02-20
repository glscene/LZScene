program Clothify;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, GLScene_RunTime, Interfaces,
  fClothify in 'fClothify.pas'{frmClothify};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClothify, frmClothify);
  Application.Run;
end.
