program Grass;

uses
  Forms, Interfaces,
  umainform in 'umainform.pas' {MainForm};


begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
