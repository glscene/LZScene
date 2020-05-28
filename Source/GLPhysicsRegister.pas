//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  DesignTime registration code for the Physics Managers

  History:
    12/01/16 - PW - Combined ODE&NGD register procedures
    18/06/03 - SG - Creation.
}

unit GLPhysicsRegister;

interface

uses
  Classes,
  GLPhysics;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLS-Physics',[TGLPhysicsManager]);
end;

end.
