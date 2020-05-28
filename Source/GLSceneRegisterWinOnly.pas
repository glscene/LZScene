//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Contains registration for Design-Time Lazarus Windows-Only units.
   Because of Lazarus'es limitations, these need to be separated from the main
   GLSceneRegister.pas

   History : 
       07/01/10 - DaStr - Removed GLLCLFullScreenViewer because it became
                              cross-platform (thanks Predator)
       24/11/09 - DanB - Added some more windows only units
       22/11/09 - DaStr - Initial version (by Predator)
    
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
