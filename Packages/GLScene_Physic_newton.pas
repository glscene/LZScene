{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_Physic_newton;

{$warn 5023 off : no warning about unused units}
interface

uses
  GLNGDManager, NewtonImport, GLPhysicNewtonRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLPhysicNewtonRegister', @GLPhysicNewtonRegister.Register);
end;

initialization
  RegisterPackage('GLScene_Physic_newton', @Register);
end.
