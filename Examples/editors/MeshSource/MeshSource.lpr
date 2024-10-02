program MeshSource;

uses
  Forms, Interfaces,
  umainform in 'umainform.pas' {MainForm};


begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
