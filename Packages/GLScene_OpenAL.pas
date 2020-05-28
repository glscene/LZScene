{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_OpenAL;

{$warn 5023 off : no warning about unused units}
interface

uses
  GLSMOpenAL, GLSound, GLSoundFileObjects, GLSoundFileMP3, GLSoundFileOGG, 
  GLSoundFileWAV, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSMOpenAL', @GLSMOpenAL.Register);
end;

initialization
  RegisterPackage('GLScene_OpenAL', @Register);
end.
