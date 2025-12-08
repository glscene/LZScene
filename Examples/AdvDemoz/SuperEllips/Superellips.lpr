program Superellips;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces,
  MAIN in 'main.pas' {MainForm},
  CHILDWIN in 'childWin.PAS' {MDIChild},
  about in 'about.pas' {AboutBox};

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
