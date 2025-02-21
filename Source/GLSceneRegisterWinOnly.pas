//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
   Contains registration for Design-Time Lazarus Windows-Only units.
   Because of Lazarus'es limitations, these need to be separated from the main
   GLSceneRegister.pas
}

unit GLSceneRegisterWinOnly;

{$IFNDEF MSWINDOWS}{$Message Error 'Unit not supported'}{$ENDIF}

interface

uses
  Classes, 
  GLSceneRegister, 
  GLStrings,
  GLSpaceText,
  GLAVIRecorder, 
  GLJoystick, 
  GLScreenSaver, 
  GLSMWaveOut, 
  LResources;

procedure Register;

//=============================================================
implementation
//=============================================================

procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLSMWaveOut
                        ]);

   RegisterComponents('GLScene Utils',
                      [TGLAVIRecorder,  TGLJoystick, TGLScreenSaver
                      ]);

end;

initialization

   {$I ../../resources/nonGLScene.lrs}

   with ObjectManager do begin
      RegisterSceneObject(TGLSpaceText, 'SpaceText', glsOCDoodad, HInstance);
   end;

finalization
end.
