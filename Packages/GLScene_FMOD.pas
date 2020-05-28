{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_FMOD;

interface

uses
  GLSMFMOD, fmodpresets, fmodtypes, fmoderrors, fmoddyn, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSMFMOD', @GLSMFMOD.Register);
end;

initialization
  RegisterPackage('GLScene_FMOD', @Register);
end.
