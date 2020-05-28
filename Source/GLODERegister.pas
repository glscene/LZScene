//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  GLODERegister - Design time registration code for the ODE Manager

  History:
    18/06/03 - SG - Creation.
}
unit GLODERegister;

interface

uses
  Classes, GLODEManager;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// Register
//
procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEJointList, TODEJoints, TODEElements]);
  RegisterComponents('GLScene',[TGLODEManager,TGLODEJointList]);
end;

end.
