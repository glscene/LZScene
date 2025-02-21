//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
  DesignTime registration code for the Physics Managers
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
