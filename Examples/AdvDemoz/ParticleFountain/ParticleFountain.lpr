// Fountain v1.1
// Fountain demo created by Dave Gravel
// http://www.k00m.sexidude.com
// lucifers23@hotmail.com
{******************************************************************************}
 // [2005-06-09]: Fountain Unit last change by Dave Gravel
 // [2014-07-21]: Fountain Unit last changed by Pavel Vassiliev
{******************************************************************************}
program ParticleFountain;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {Form1},
  Fountain_Unit in 'Fountain_Unit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
