{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_SDL;

interface

uses
  GLSDLWindow, GLSDLContext, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSDLWindow', @GLSDLWindow.Register);
  RegisterUnit('GLSDLContext', @GLSDLContext.Register);
end;

initialization
  RegisterPackage('GLScene_SDL', @Register);
end.
