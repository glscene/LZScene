
// This unit is part of the GLScene Project, http://glscene.org

{
   Support classes for loading various fileformats.
   These classes work together like vector file formats or Delphi's TGraphic classes.

  History :
       17/11/09 - DaStr - Improved Unix compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
       13/07/09 - DanB - replaced sAllFilter with glsAllFilter (for FPC)
       30/05/09 - DanB - TGLSoundSampling.WaveFormat now returns correct nBlockAlign, cbSize.
       16/10/08 - UweR - Compatibility fix for Delphi 2009
       07/06/07 - DaStr - Added $I GLScene.inc
       26/01/05 - JAJ - Removed leak formed by never freeing vSoundFileFormats.
                            Reported by Dikoe Kenguru.
       16/03/01 - Egg - TGLWAVFile.Capabilities
       16/07/00 - Egg - Made use of new TGLDataFile class
       09/06/00 - Egg - Added WAVDataSize
       04/06/00 - Egg - Creation

}
unit GLSoundFileObjects;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,{$IFDEF MSWINDOWS}MMSystem,{$ENDIF}
  GLApplicationFileIO, GLCrossPlatform;

type
  EGLSoundException = class(Exception);
  EGLInvalidSoundFile = class(EGLSoundException);
  // TGLSoundSampling

  { Defines a sound sampling quality. }
  TGLSoundSampling = class(TPersistent)
  private

    FOwner: TPersistent;
    FFrequency: integer;
    FNbChannels: integer;
    FBitsPerSample: integer;

  protected

    function GetOwner: TPersistent; override;

  public

    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function BytesPerSec: integer;
    function BytesPerSample: integer;



        {$IFDEF MSWINDOWS}
    function WaveFormat: TWaveFormatEx;
        {$ENDIF}
  published

    { Sampling frequency in Hz (= samples per sec) }
    property Frequency: integer read FFrequency write FFrequency default 22050;
         { Nb of sampling channels.
            1 = mono, 2 = stereo, etc. }
    property NbChannels: integer read FNbChannels write FNbChannels default 1;
         { Nb of bits per sample.
            Common values are 8 and 16 bits. }
    property BitsPerSample: integer read FBitsPerSample
      write FBitsPerSample default 8;
  end;

  // TGLSoundFile

   { Abstract base class for different Sound file formats.
      The actual implementation for these files (WAV, RAW...) must be done
      seperately. The concept for TGLSoundFile is very similar to TGraphic
      (see Delphi Help).
      Default implementation for LoadFromFile/SaveToFile are to directly call the
      relevent stream-based methods, ie. you will just have to override the stream
      methods in most cases. }
  TGLSoundFile = class(TGLDataFile)
  private

    FSampling: TGLSoundSampling;

  protected

    procedure SetSampling(const val: TGLSoundSampling);
    function CheckFormat(stream: TStream): boolean; virtual;
    procedure LoadFromMemory(Memory: TStream); virtual;

  public

    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure PlayOnWaveOut; dynamic;

    { Returns a pointer to the sample data viewed as an in-memory WAV File. }
    function WAVData: Pointer; virtual; abstract;
    { Returns the size (in bytes) of the WAVData. }
    function WAVDataSize: integer; virtual; abstract;
    { Returns a pointer to the sample data viewed as an in-memory PCM buffer. }
    function PCMData: Pointer; virtual; abstract;
    { Length of PCM data, in bytes. }
    function LengthInBytes: integer; virtual; abstract;
    { Nb of intensity samples in the sample. }
    function LengthInSamples: integer;
    { Length of play of the sample at nominal speed in seconds. }
    function LengthInSec: single;
    function getSoundData: PByte; virtual;
    function getSoundDataSize: int64; virtual;

    property Sampling: TGLSoundSampling read FSampling write SetSampling;
  end;

  TGLSoundFileClass = class of TGLSoundFile;

  // TGLSoundFileFormat

  TGLSoundFileFormat = record
    SoundFileClass: TGLSoundFileClass;
    Extension: string;
    Description: string;
    DescResID: integer;
  end;
  PSoundFileFormat = ^TGLSoundFileFormat;

  // TGLSoundFileFormatsList

  TGLSoundFileFormatsList = class(TList)
  public

    destructor Destroy; override;
    procedure Add(const Ext, Desc: string; DescID: integer;
      AClass: TGLSoundFileClass);
    function FindExt(Ext: string): TGLSoundFileClass;
    procedure Remove(AClass: TGLSoundFileClass);
    procedure BuildFilterStrings(SoundFileClass: TGLSoundFileClass;
      out Descriptions, Filters: string);
  end;

function GetGLSoundFileFormats: TGLSoundFileFormatsList;
procedure RegisterSoundFileFormat(const AExtension, ADescription: string;
  AClass: TGLSoundFileClass);
procedure UnregisterSoundFileClass(AClass: TGLSoundFileClass);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


var
  vSoundFileFormats: TGLSoundFileFormatsList;

// GeTGLSoundFileFormats

function GetGLSoundFileFormats: TGLSoundFileFormatsList;
begin
  if not Assigned(vSoundFileFormats) then
    vSoundFileFormats := TGLSoundFileFormatsList.Create;
  Result := vSoundFileFormats;
end;

// RegisterSoundFileFormat

procedure RegisterSoundFileFormat(const AExtension, ADescription: string;
  AClass: TGLSoundFileClass);
begin
  RegisterClass(AClass);
  GetGLSoundFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterSoundFileClass

procedure UnregisterSoundFileClass(AClass: TGLSoundFileClass);
begin
  if Assigned(vSoundFileFormats) then
    vSoundFileFormats.Remove(AClass);
end;

// ------------------
// ------------------ TGLSoundSampling ------------------
// ------------------

// Create

constructor TGLSoundSampling.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FFrequency := 22050;
  FNbChannels := 1;
  FBitsPerSample := 8;
end;

// Destroy

destructor TGLSoundSampling.Destroy;
begin
  inherited Destroy;
end;



procedure TGLSoundSampling.Assign(Source: TPersistent);
begin
  if Source is TGLSoundSampling then
  begin
    FFrequency := TGLSoundSampling(Source).Frequency;
    FNbChannels := TGLSoundSampling(Source).NbChannels;
    FBitsPerSample := TGLSoundSampling(Source).BitsPerSample;
  end
  else
    inherited;
end;

// GetOwner

function TGLSoundSampling.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// BytesPerSec

function TGLSoundSampling.BytesPerSec: integer;
begin
  Result := (FFrequency * FBitsPerSample * FNbChannels) shr 3;
end;

// BytesPerSample

function TGLSoundSampling.BytesPerSample: integer;
begin
  Result := FBitsPerSample shr 3;
end;

{$IFDEF MSWINDOWS}
// WaveFormat

function TGLSoundSampling.WaveFormat: TWaveFormatEx;
begin
  Result.nSamplesPerSec := Frequency;
  Result.nChannels := NbChannels;
  Result.wFormatTag := Wave_Format_PCM;
  Result.nAvgBytesPerSec := BytesPerSec;
  Result.wBitsPerSample := BitsPerSample;
  Result.nBlockAlign := NbChannels * BytesPerSample;
  Result.cbSize := 0;
end;

{$ENDIF}

// ------------------
// ------------------ TGLSoundFile ------------------
// ------------------

// Create

constructor TGLSoundFile.Create(AOwner: TPersistent);
begin
  inherited;
  FSampling := TGLSoundSampling.Create(Self);
end;

// Destroy

destructor TGLSoundFile.Destroy;
begin
  FSampling.Free;
  inherited;
end;

// SetSampling

procedure TGLSoundFile.SetSampling(const val: TGLSoundSampling);
begin
  FSampling.Assign(val);
end;

function TGLSoundFile.CheckFormat(stream: TStream): boolean;
begin
  Result := True;
end;

procedure TGLSoundFile.LoadFromMemory(Memory: TStream);
begin
  // Do Nothing
end;

// PlayOnWaveOut

procedure TGLSoundFile.PlayOnWaveOut;
begin
  //   GLSoundFileObjects.PlayOnWaveOut(PCMData, LengthInSamples, Sampling);
end;

// LengthInSamples

function TGLSoundFile.LengthInSamples: integer;
var
  d: integer;
begin
  d := Sampling.BytesPerSample * Sampling.NbChannels;
  if d > 0 then
    Result := LengthInBytes div d
  else
    Result := 0;
end;

// LengthInSec

function TGLSoundFile.LengthInSec: single;
begin
  Result := LengthInBytes / Sampling.BytesPerSec;
end;

function TGLSoundFile.getSoundData: PByte;
begin
  Result := nil;
end;

function TGLSoundFile.getSoundDataSize: int64;
begin
  Result := 0;
end;

// ------------------
// ------------------ TGLSoundFileFormatsList ------------------
// ------------------

// Destroy

destructor TGLSoundFileFormatsList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PSoundFileFormat(Items[i]));
  inherited;
end;

// Add

procedure TGLSoundFileFormatsList.Add(const Ext, Desc: string;
  DescID: integer; AClass: TGLSoundFileClass);
var
  newRec: PSoundFileFormat;
begin
  New(newRec);
  with newRec^ do
  begin
    Extension := AnsiLowerCase(Ext);
    SoundFileClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(NewRec);
end;

// FindExt

function TGLSoundFileFormatsList.FindExt(Ext: string): TGLSoundFileClass;
var
  i: integer;
begin
  Ext := AnsiLowerCase(Ext);
  for I := Count - 1 downto 0 do
    with PSoundFileFormat(Items[I])^ do
      if (Extension = Ext) or ('.' + Extension = Ext) then
      begin
        Result := SoundFileClass;
        Exit;
      end;
  Result := nil;
end;

// Remove

procedure TGLSoundFileFormatsList.Remove(AClass: TGLSoundFileClass);
var
  i: integer;
  p: PSoundFileFormat;
begin
  for I := Count - 1 downto 0 do
  begin
    P := PSoundFileFormat(Items[I]);
    if P^.SoundFileClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

// BuildFilterStrings

procedure TGLSoundFileFormatsList.BuildFilterStrings(SoundFileClass: TGLSoundFileClass;
  out Descriptions, Filters: string);
var
  c, i: integer;
  p: PSoundFileFormat;
begin
  Descriptions := '';
  Filters := '';
  C := 0;
  for I := Count - 1 downto 0 do
  begin
    P := PSoundFileFormat(Items[I]);
    if P^.SoundFileClass.InheritsFrom(SoundFileClass) and (P^.Extension <> '') then
      with P^ do
      begin
        if C <> 0 then
        begin
          Descriptions := Descriptions + '|';
          Filters := Filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s',
          [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(C);
      end;
  end;
  if C > 1 then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s', [glsAllFilter, Filters, Descriptions]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

finalization

  FreeAndNil(vSoundFileFormats);

end.
