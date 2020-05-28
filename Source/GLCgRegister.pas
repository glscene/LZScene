//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Registration unit for CG shader.

    History : 
       11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
       23/02/07 - DaStr - Initial version

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
