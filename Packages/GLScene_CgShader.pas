{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_CgShader; 

interface

uses
   GLCgRegister, GLCgShader, cg, cgGL, GLCgBombShader, 
   cgPostTransformationShader, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLCgRegister', @GLCgRegister.Register); 
end; 

initialization
  RegisterPackage('GLScene_CgShader', @Register); 
end.
