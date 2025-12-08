program FractalLandscape;

{$MODE Delphi}

uses
  Forms, Interfaces,
  dlgFracLanU in 'dlgFracLanU.pas' {dlgFracLan},
  dlgProgressU in 'dlgProgressU.pas' {dlgProgress};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgFracLan, dlgFracLan);
  Application.CreateForm(TdlgProgress, dlgProgress);
  Application.Run;
end.
