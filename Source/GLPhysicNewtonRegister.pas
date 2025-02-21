//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
  Design time registration code for the Newton Manager.
}

unit GLPhysicNewtonRegister;

interface

uses
  Classes, 
  GLNGDManager;

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
