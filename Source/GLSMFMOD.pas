//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   FMOD based sound-manager (http://www.fmod.org/, free for freeware).

   Unsupported feature(s) : 
       sound source velocity
       looping (sounds are played either once or forever)
       sound cones
    

	 History :  
       07/01/10 - DaStr - Fixed a bug with an initial Paused or Muted state of
                              sound source and with sscSample in aSource.Changes
       17/03/08 - mrqzzz - Fixed "Consant cannot be pased as var parameter" in NotifyEnvironmentChanged
       15/03/08 - DaStr - Updated to Fmod v3.7.4 (thanks Chen, Pei)
       07/06/07 - DaStr - Added $I GLScene.inc
       18/10/03 - EG - Dynamic support is back
       18/09/03 - ARH - updated for fmod 3.7
       24/09/02 - EG - FMOD activation errors no longer result in Asserts (ignored)
       28/08/02 - EG - Fixed EAX capability detection
       27/08/02 - EG - Now uses dynamically linked version by Steve Williams,
                          Added support for EAX environments
       26/08/02 - EG - Updated to FMOD 3.6
       27/02/02 - EG - Updated to FMOD 3.5, added 3D Factors
       05/02/02 - EG - Updated to FMOD 3.4, now uses DSound by default
       13/01/01 - EG - Updated for API 3.3 compatibility
	    09/06/00 - EG - Creation
	 
}
unit GLSMFMOD;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,

  //GLScene
  GLSound, GLScene, GLVectorGeometry,
  FMod, FmodTypes, FmodPresets;

type

	// TGLSMFMOD
	//
	TGLSMFMOD = class (TGLSoundManager)
	   private
	       
         FActivated : Boolean;
         FEAXCapable : Boolean; // not persistent

	   protected
	       
	      function DoActivate : Boolean; override;
	      procedure DoDeActivate; override;
         procedure NotifyMasterVolumeChange; override;
         procedure Notify3DFactorsChanged; override;
         procedure NotifyEnvironmentChanged; override;

         procedure KillSource(aSource : TGLBaseSoundSource); override;
         procedure UpdateSource(aSource : TGLBaseSoundSource); override;
         procedure MuteSource(aSource : TGLBaseSoundSource; muted : Boolean); override;
         procedure PauseSource(aSource : TGLBaseSoundSource; paused : Boolean); override;

         function GetDefaultFrequency(aSource : TGLBaseSoundSource) : Integer;

      public
	       
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         procedure UpdateSources; override;

         function CPUUsagePercent : Single; override;
         function EAXSupported : Boolean; override;

	   published
	       
         property MaxChannels default 32;
	end;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------


type
   TFMODInfo =  record
      channel : Integer;
      pfs : PFSoundSample;
   end;
   PFMODInfo = ^TFMODInfo;

procedure Register;
begin
  RegisterComponents('GLScene', [TGLSMFMOD]);
end;

// VectorToFMODVector
//
procedure VectorToFMODVector(const aVector : TVector; var aFMODVector : TFSoundVector);
begin
  aFMODVector.x:=aVector.X;
  aFMODVector.y:=aVector.Y;
  aFMODVector.z:=-aVector.Z;
end;

// ------------------
// ------------------ TGLSMFMOD ------------------
// ------------------

// Create
//
constructor TGLSMFMOD.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   MaxChannels:=32;
end;

// Destroy
//
destructor TGLSMFMOD.Destroy;
begin
	inherited Destroy;
end;

// DoActivate
//
function TGLSMFMOD.DoActivate : Boolean;
var
   cap : Cardinal;
begin
   FMOD_Load(nil);
   {$IFDEF MSWindows}
   if not FSOUND_SetOutput(FSOUND_OUTPUT_DSOUND) then begin
      Result:=False;
      Exit;
   end;
  {$ENDIF}
  {$IFDEF LINUX}
  if not FSOUND_SetOutput(FSOUND_OUTPUT_ALSA) then begin
     Result:=False;
     Exit;
  end;
  {$ENDIF}
   if not FSOUND_SetDriver(0) then begin
      Result:=False;
      Exit;
   end;
   cap:=0;
   if FSOUND_GetDriverCaps(0, cap) then
      FEAXCapable:=((cap and (FSOUND_CAPS_EAX2 or FSOUND_CAPS_EAX3))>0)
   else Assert(False, 'Failed to retrieve driver Caps.');
   if not FSOUND_Init(OutputFrequency, MaxChannels, 0) then
      Assert(False, 'FSOUND_Init failed.');
   FActivated:=True;
   NotifyMasterVolumeChange;
   Notify3DFactorsChanged;
   if Environment<>seDefault then
      NotifyEnvironmentChanged;
   Result:=True;
end;

// DoDeActivate
//
procedure TGLSMFMOD.DoDeActivate;
begin
   FSOUND_StopSound(FSOUND_ALL);
   FSOUND_Close;
   FMOD_Unload;
   FEAXCapable:=False;
end;

// NotifyMasterVolumeChange
//
procedure TGLSMFMOD.NotifyMasterVolumeChange;
begin
   if FActivated then
      FSOUND_SetSFXMasterVolume(Round(MasterVolume*255));
end;

// Notify3DFactorsChanged
//
procedure TGLSMFMOD.Notify3DFactorsChanged;
begin
   if FActivated then begin
      FSOUND_3D_SetDistanceFactor(DistanceFactor);
      FSOUND_3D_SetRolloffFactor(RollOffFactor);
      FSOUND_3D_SetDopplerFactor(DopplerFactor);
   end;
end;

// NotifyEnvironmentChanged
//
procedure TGLSMFMOD.NotifyEnvironmentChanged;
var
   SoundRevProps:TFSoundReverbProperties;
begin
   if FActivated and EAXSupported then begin
      case Environment of
         seDefault :          SoundRevProps := FSOUND_PRESET_GENERIC;
         sePaddedCell :       SoundRevProps := FSOUND_PRESET_PADDEDCELL;
         seRoom :             SoundRevProps := FSOUND_PRESET_ROOM;
         seBathroom :         SoundRevProps := FSOUND_PRESET_BATHROOM;
         seLivingRoom :       SoundRevProps := FSOUND_PRESET_LIVINGROOM;
         seStoneroom :        SoundRevProps := FSOUND_PRESET_STONEROOM;
         seAuditorium :       SoundRevProps := FSOUND_PRESET_AUDITORIUM;
         seConcertHall :      SoundRevProps := FSOUND_PRESET_CONCERTHALL;
         seCave :             SoundRevProps := FSOUND_PRESET_CAVE;
         seArena :            SoundRevProps := FSOUND_PRESET_ARENA;
         seHangar :           SoundRevProps := FSOUND_PRESET_HANGAR;
         seCarpetedHallway :  SoundRevProps := FSOUND_PRESET_CARPETTEDHALLWAY;
         seHallway :          SoundRevProps := FSOUND_PRESET_HALLWAY;
         seStoneCorridor :    SoundRevProps := FSOUND_PRESET_STONECORRIDOR;
         seAlley :            SoundRevProps := FSOUND_PRESET_ALLEY;
         seForest :           SoundRevProps := FSOUND_PRESET_FOREST;
         seCity :             SoundRevProps := FSOUND_PRESET_CITY;
         seMountains :        SoundRevProps := FSOUND_PRESET_MOUNTAINS;
         seQuarry :           SoundRevProps := FSOUND_PRESET_QUARRY;
         sePlain :            SoundRevProps := FSOUND_PRESET_PLAIN;
         seParkingLot :       SoundRevProps := FSOUND_PRESET_PARKINGLOT;
         seSewerPipe :        SoundRevProps := FSOUND_PRESET_SEWERPIPE;
         seUnderWater :       SoundRevProps := FSOUND_PRESET_UNDERWATER;
         seDrugged :          SoundRevProps := FSOUND_PRESET_DRUGGED;
         seDizzy :            SoundRevProps := FSOUND_PRESET_DIZZY;
         sePsychotic :        SoundRevProps := FSOUND_PRESET_PSYCHOTIC;
      else
         Assert(False);
      end;
      FSOUND_Reverb_SetProperties(SoundRevProps);
   end;
end;

// KillSource
//
procedure TGLSMFMOD.KillSource(aSource : TGLBaseSoundSource);
var
   p : PFMODInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      aSource.ManagerTag:=0;
      if p.channel<>-1 then
         if not FSOUND_StopSound(p.channel) then
            Assert(False, IntToStr(Integer(p)));
      FSOUND_Sample_Free(p.pfs);
      FreeMem(p);
   end;
end;

// UpdateSource
//
procedure TGLSMFMOD.UpdateSource(aSource : TGLBaseSoundSource);
var
   p : PFMODInfo;
   objPos, objVel : TVector;
   position, velocity : TFSoundVector;
begin
   if (sscSample in aSource.Changes) then
   begin
     KillSource(aSource);
   end;

   if (aSource.Sample=nil) or (aSource.Sample.Data=nil) or
      (aSource.Sample.Data.WAVDataSize=0) then Exit;
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      if not FSOUND_IsPlaying(p.channel) then begin
         p.channel:=-1;
         aSource.Free;
         Exit;
      end;
   end else begin
      p:=AllocMem(SizeOf(TFMODInfo));
      p.channel:=-1;
      p.pfs:=FSOUND_Sample_Load(FSOUND_FREE, aSource.Sample.Data.WAVData,
                                FSOUND_HW3D+FSOUND_LOOP_OFF+FSOUND_LOADMEMORY,
                                0, aSource.Sample.Data.WAVDataSize);

      if aSource.NbLoops>1 then
         FSOUND_Sample_SetMode(p.pfs, FSOUND_LOOP_NORMAL);
      FSOUND_Sample_SetMinMaxDistance(p.pfs, aSource.MinDistance, aSource.MaxDistance);
      aSource.ManagerTag:=Integer(p);
   end;
   if aSource.Origin<>nil then begin
      objPos:=aSource.Origin.AbsolutePosition;
      objVel:=NullHmgVector;
   end else begin
      objPos:=NullHmgPoint;
      objVel:=NullHmgVector;
   end;
   VectorToFMODVector(objPos, position);
   VectorToFMODVector(objVel, velocity);
   if p.channel=-1 then
      p.channel:=FSOUND_PlaySound(FSOUND_FREE, p.pfs);
   if p.channel<>-1 then begin
      FSOUND_3D_SetAttributes(p.channel, @position, @velocity);
      FSOUND_SetVolume(p.channel, Round(aSource.Volume*255));
      FSOUND_SetMute(p.channel, aSource.Mute);
      FSOUND_SetPaused(p.channel, aSource.Pause);
      FSOUND_SetPriority(p.channel, aSource.Priority);
      if aSource.Frequency>0 then
         FSOUND_SetFrequency(p.channel, aSource.Frequency);
   end else aSource.Free;

   inherited UpdateSource(aSource);
end;

// MuteSource
//
procedure TGLSMFMOD.MuteSource(aSource : TGLBaseSoundSource; muted : Boolean);
var
   p : PFMODInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      FSOUND_SetMute(p.channel, muted);
   end;
end;

// PauseSource
//
procedure TGLSMFMOD.PauseSource(aSource : TGLBaseSoundSource; paused : Boolean);
var
   p : PFMODInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      FSOUND_SetPaused(p.channel, paused);
   end;
end;

// UpdateSources
//
procedure TGLSMFMOD.UpdateSources;
var
   objPos, objVel, objDir, objUp : TVector;
   position, velocity, fwd, top : TFSoundVector;
begin
   // update listener
   ListenerCoordinates(objPos, objVel, objDir, objUp);
   VectorToFMODVector(objPos, position);
   VectorToFMODVector(objVel, velocity);
   VectorToFMODVector(objDir, fwd);
   VectorToFMODVector(objUp, top);
   FSOUND_3D_Listener_SetAttributes(@position, @velocity,
                                    fwd.x, fwd.y, fwd.z,
                                    top.x, top.y, top.z);
   // update sources
   inherited;
   FSOUND_Update;
end;

// CPUUsagePercent
//
function TGLSMFMOD.CPUUsagePercent : Single;
begin
   Result:=FSOUND_GetCPUUsage;
end;

// EAXSupported
//
function TGLSMFMOD.EAXSupported : Boolean;
begin
   Result:=FEAXCapable;
end;

// GetDefaultFrequency
//
function TGLSMFMOD.GetDefaultFrequency(aSource : TGLBaseSoundSource): integer;
var
   p : PFMODInfo;
   dfreq, dVol, dPan, dPri : Integer;
begin
   try
      p:=PFMODInfo(aSource.ManagerTag);
      FSOUND_Sample_GetDefaults(p.pfs, dFreq, dVol, dPan, dPri);
      Result:=dFreq;
   except
      Result:=-1;
   end;
end;

end.

