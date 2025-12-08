program simpleshadow;

{$MODE Delphi}

uses
  Forms, Interfaces,
  u_Main in 'u_Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
