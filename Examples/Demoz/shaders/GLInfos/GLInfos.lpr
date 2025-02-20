program GLInfos;

uses
  Forms, Interfaces,
  umainform in 'umainform.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
