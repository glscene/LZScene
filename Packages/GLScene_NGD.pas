{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_NGD; 

interface

uses
  GLNewtonRegister, GLNGDManager, NewtonImport, NewtonImport_JointLibrary, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLNewtonRegister', @GLNewtonRegister.Register); 
end; 

initialization
  RegisterPackage('GLScene_NGD', @Register); 
end.
