{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_WinOnly;

interface

uses
  GLJoystick, GLScreenSaver, GLSMWaveOut, GLAVIRecorder, GLSVfw, GLSpaceText, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('GLScene_WinOnly', @Register);
end.
