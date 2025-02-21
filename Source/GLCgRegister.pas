//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
   Registration unit for CG shader.
}
unit GLCgRegister;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes,

  propedits, GLSceneRegisterLCL,
  GLScene
  GLMaterial,

  // CG
  Cg, CgGL, GLCgShader, GLCgBombShader;

procedure Register;

implementation

procedure Register;
begin
  // Register components.
  RegisterComponents('GLScene Shaders', [TCgShader, TGLCgBombShader]);

  // Register property editors.
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLCgBombShader, '', TGLLibMaterialNameProperty);
end;

end.
