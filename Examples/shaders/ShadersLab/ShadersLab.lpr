//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  GLSLShaderLab Demo : Demo that show how to use some GLSLShader.
  If you want to use your own model, take care that models need to have UV Coordinates

  History :
  01/12/15 - J.Delauney - Creation

}
program ShadersLab;

uses
  Forms, Interfaces,
  UMainForm in 'UMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
