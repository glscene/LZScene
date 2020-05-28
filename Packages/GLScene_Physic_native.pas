{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_Physic_native;

{$warn 5023 off : no warning about unused units}
interface

uses
  GLForceFields, GLForces, GLInertias, GLJoints, GLPhysics, GLPhysicsRegister, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLPhysicsRegister', @GLPhysicsRegister.Register);
end;

initialization
  RegisterPackage('GLScene_Physic_native', @Register);
end.
