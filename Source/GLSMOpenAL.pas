//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   OpenAL based sound-manager (http://www.openal.org).
   OpenAL drivers can be download from the OpenAL site or your soundcard
   manufacturer's website.

   Unsupported feature(s) : 
       Accepts only simple *uncompressed* WAV files (8/16 bits, mono/stereo)
       Dynamic loading/unloading
       Global 3D parameters
       Environments
       CPUUsagePercent
       No system in place to limit number of sources playing simultaneously,
          can crash if too playing at once.
       ???
    

	 History :  
       11/11/09 - DaStr - Added $I GLScene.inc
       16/10/08 - UweR - Compatibility fix for Delphi 2009
       25/03/08 - DanB - Added design-time support, linked to new OpenAL headers
                            (see OpenAL.pas).
       ??/??/03 - Mrqzz - Creation
	 
}
unit GLSMOpenAL;

interface

{$I GLScene.inc}

uses
   Classes, GLSound, GLScene, SysUtils, GLSoundFileObjects;

type

	// TGLSMOpenAL
	//
	TGLSMOpenAL = class (TGLSoundManager)
      private
         FActivated : Boolean;      
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

         function GetALFormat(sampling : TGLSoundSampling) : Integer;
         
      public
	       
         constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         procedure UpdateSources; override;

         function EAXSupported : Boolean; override;
	end;

   EOpenALError = Exception;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses
  Forms, Dialogs,
  GLVectorGeometry, OpenAL{al, alut, alTypes};

procedure Register;
begin
   RegisterComponents('GLScene', [TGLSMOpenAL]);
end;

//checks for an error and raises an exception if necessary
procedure CheckOpenALError;
var
   error : integer;
begin
   error:=alGetError;
   if error<>AL_NO_ERROR then
      raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
end;

//clears the error-states
procedure ClearOpenALError;
begin
   alGetError;
end;

// ------------------
// ------------------ TGLSMOpenAL ------------------
// ------------------

// Create
//
constructor TGLSMOpenAL.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
end;

// Destroy
//
destructor TGLSMOpenAL.Destroy;
begin
	inherited Destroy;
end;

// DoActivate
//
function TGLSMOpenAL.DoActivate : Boolean;
var
     dummy: array of PALbyte;
begin
     Result:=false;

     // Setup OpenAL
     if not InitOpenAL() then
       Exit;
     dummy:= nil;
     alutInit(nil, dummy);
     CheckOpenALError;
     alDistanceModel(AL_INVERSE_DISTANCE);
     CheckOpenALError;
     ReadOpenALExtensions();

     // Set any global states
     FActivated:=true;
     NotifyMasterVolumeChange;
     Notify3DFactorsChanged;
     if Environment<>seDefault then
          NotifyEnvironmentChanged;

     Result:=True;
end;

// DoDeActivate
//
procedure TGLSMOpenAL.DoDeActivate;
var
  i:integer;
begin
  FActivated:=false;
  for i := 0 to Sources.Count - 1 do
  begin
    Sources[i].Sample.ManagerTag := 0;
  end;
  alutExit;
end;

// NotifyMasterVolumeChange
//
procedure TGLSMOpenAL.NotifyMasterVolumeChange;
begin
  if FActivated then
  begin
    alListenerf(AL_GAIN,MasterVolume);
  end;
end;

// Notify3DFactorsChanged
//
procedure TGLSMOpenAL.Notify3DFactorsChanged;
begin
  if FActivated then
  begin
    alDopplerFactor(DopplerFactor);
  end;
end;

// NotifyEnvironmentChanged
//
procedure TGLSMOpenAL.NotifyEnvironmentChanged;
begin
  if FActivated then
  begin
    // check extension is available + update
    if EAXSupported then
    begin
      // nothing yet
    end;
  end;
end;

// KillSource
//
procedure TGLSMOpenAL.KillSource(aSource : TGLBaseSoundSource);
var
  i, currentBufferTag, bufferCount:integer;
begin
  if aSource.ManagerTag<>0 then
  begin
    alSourceStop(ASource.ManagerTag);
    alDeleteSources(1, PALuint(@ASource.ManagerTag));
    ASource.ManagerTag:=0;

    // We can't just delete buffer, because other sources may be using it
    // so we count how many sources are using, then delete if it's the only one
    // using.
    // Same for ASource.Sample.ManagerTag, we set to zero once it's no longer
    // being used by any other sources

    currentBufferTag:=ASource.Sample.ManagerTag;
    bufferCount:=0;
    if currentBufferTag<>0 then
    begin
      for i := 0 to Sources.Count - 1 do
      begin
        if Sources[i].Sample.ManagerTag = currentBufferTag then
        begin
          bufferCount:=bufferCount+1;
        end;
      end;
      if bufferCount=1 then
      begin
        alDeleteBuffers(1, PALuint(@ASource.Sample.ManagerTag));
        ASource.Sample.ManagerTag := 0;
      end;
    end;
  end;
end;

// UpdateSource
//
procedure TGLSMOpenAL.UpdateSource(aSource : TGLBaseSoundSource);
var
  a: TALint;
begin
     // Clear any errors we may enter into procedure with
     ClearOpenALError;

     // Create an OpenAL source object if needed, and put ID into aSource.ManagerTag
     if aSource.ManagerTag = 0 then begin
          alGenSources(1, PALuint(@aSource.managerTag));
          CheckOpenALError;
     end
     else begin
       // Check to see if source has stopped, if so free it as limited number of sources allowed
       alGetSourcei(aSource.ManagerTag,AL_SOURCE_STATE,@a);
       CheckOpenALError;
       if a=AL_STOPPED then
       begin
         aSource.Free;
         Exit;
       end;
     end;

     //if sscTransformation in aSource.Changes then begin
          alSourcefv(aSource.ManagerTag, AL_POSITION, PALFloat(aSource.Origin.Position.asAddress));
          CheckOpenALError;
          alSourcefv(aSource.ManagerTag, AL_DIRECTION, PALFloat(aSource.Origin.Direction.asAddress));
          CheckOpenALError;
     //end;

     if aSource.SoundName <> '' then begin

          // If the sample doesn't have a reference to an OpenAL buffer
          // we need to create a buffer, and load the sample data into it
          if (aSource.Sample.ManagerTag = 0)and Assigned(aSource.Sample.Data) then begin
               alGenBuffers(1, PALuint(@aSource.sample.ManagerTag));
               CheckOpenALError;
               // fill buffer (once buffer filled, can't fill buffer again, unless no other sources playing)
               alBufferData(aSource.sample.ManagerTag,
                            GetALFormat(aSource.sample.Sampling),
                            aSource.sample.Data.PCMData,
                            aSource.sample.data.LengthInBytes,
                            aSource.Sample.Data.Sampling.Frequency);
               CheckOpenALError;

          end;

          if (sscSample in aSource.Changes) and assigned(aSource.Sample.Data) then begin

               // Associate buffer with source, buffer may have either been recently
               // created, or already existing if being used by another source
               alSourcei(aSource.ManagerTag, AL_BUFFER, aSource.sample.ManagerTag);
               CheckOpenALError;

               // If NbLoops>1 the source will constantly loop the sample, otherwise only play once
               alSourcei(aSource.managerTag, AL_LOOPING, Integer(aSource.NbLoops>1));
               CheckOpenALError;

               // Start the source playing!
               alSourcePlay(aSource.ManagerTag);
               CheckOpenALError;
          end;
     end;

     if sscStatus in aSource.changes then begin
          alSourcef(aSource.ManagerTag,AL_PITCH,1.0);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag,AL_GAIN,1.0);
          CheckOpenALError;
          alSourcef(aSource.managerTag, AL_MAX_DISTANCE, aSource.MaxDistance);
          CheckOpenALError;
          alSourcef(aSource.managerTag, AL_ROLLOFF_FACTOR, 1.0);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_REFERENCE_DISTANCE, aSource.MinDistance);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_CONE_INNER_ANGLE, aSource.InsideConeAngle);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_CONE_OUTER_ANGLE, aSource.OutsideConeAngle);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_CONE_OUTER_GAIN, aSource.ConeOutsideVolume);
     end;
     inherited UpdateSource(aSource);
end;

// MuteSource
//
procedure TGLSMOpenAL.MuteSource(aSource : TGLBaseSoundSource; muted : Boolean);
begin
     if muted then alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 0.0)
     else alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 1.0);
end;

// PauseSource
//
procedure TGLSMOpenAL.PauseSource(aSource : TGLBaseSoundSource; paused : Boolean);
begin
     if not paused then begin
          alSourceRewind(aSource.managerTag);
          alSourcePlay(aSource.ManagerTag);
     end else
          alSourcePause(aSource.ManagerTag);
end;

// UpdateSources
//
procedure TGLSMOpenAL.UpdateSources;
var
     pos, dir, up, vel: TVector;
     DirUp: array[0..5] of TALfloat; //orientation
begin
     ListenerCoordinates(pos, vel, dir, up);
     alListenerfv(AL_POSITION, PALfloat(@pos));
     alListenerfv(AL_VELOCITY, PALfloat(@vel));

     dirUp[0]:= dir.V[0];
     dirUp[1]:= dir.V[1];
     dirUp[2]:= dir.V[2];
     dirUp[3]:= up.V[0];
     dirUp[4]:= up.V[1];
     dirUp[5]:= up.V[2];
     alListenerfv(AL_ORIENTATION, PALfloat(@dirUp));

     inherited;
end;

// EAXSupported
//
function TGLSMOpenAL.EAXSupported : Boolean;
begin
     result:= alIsExtensionPresent(PAnsiChar('EAX2.0'));
end;

// GetDefaultFrequency
//
function TGLSMOpenAL.GetDefaultFrequency(aSource : TGLBaseSoundSource): integer;
begin
      Result:=-1;
end;

// GetALFormat
//
function TGLSMOpenAL.GetALFormat(sampling: TGLSoundSampling): integer;
begin
     result:= 0;
     
     //mono
     if sampling.NbChannels = 1 then case sampling.BitsPerSample of
          8: result:= AL_FORMAT_MONO8;
          16: result:= AL_FORMAT_MONO16;
     end else case sampling.BitsPerSample of //stereo
          8: result:= AL_FORMAT_STEREO8;
          16: result:= AL_FORMAT_STEREO16;
     end;
end;

end.

