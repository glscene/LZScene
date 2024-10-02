program MegaCube;

{$MODE Delphi}

uses
  Forms, Interfaces,
  u_Main in 'u_Main.pas' {Form1},
  u_simpleVBO in 'u_simpleVBO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
