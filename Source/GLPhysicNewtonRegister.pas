//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Design time registration code for the Newton Manager.

   History :  
   04/01/11 - FP - Removed Joint
   15/07/10 - FP - Creation by Franck Papouin
   
}

unit GLPhysicNewtonRegister;

interface

uses
  Classes, GLNGDManager;

procedure register;

implementation

// Register
//
procedure register;
begin
  RegisterClasses([TGLNGDManager, TGLNGDDynamic, TGLNGDStatic]);
  RegisterComponents('GLS-Physics', [TGLNGDManager]);
end;

end.
