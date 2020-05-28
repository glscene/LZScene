//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   GLScene RGBE

   History :  
   17/11/14 - PW - Renamed from RGBE.pas to GLSRGBE.pas
   15/06/10 - Yar - Fixes for Linux x64
   20/01/10 - Yar - Creation
   
}
unit GLSRGBE;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
   
  GLVectorTypes, GLVectorGeometry, GLCrossPlatform;

procedure Float2rgbe(var RGBE: TVector4b; const Red, Green, Blue: Single);
procedure Rgbe2float(var Red, Green, Blue: Single; const RGBE: TVector4b);
procedure LoadRLEpixels(Stream: TStream; Dst: PSingle;
  Scanline_width, Num_scanlines: Integer);
procedure LoadRGBEpixels(Stream: TStream; Dst: PSingle; Numpixels: Integer);

implementation

type
  ERGBEexception = class(Exception);

  { Extract exponent and mantissa from X }
procedure Frexp(X: Extended; var Mantissa: Extended; var Exponent: Integer);
{ Mantissa ptr in EAX, Exponent ptr in EDX }
{$IFDEF GLS_NO_ASM}
begin
  Exponent := 0;
  if (X <> 0) then
    if (Abs(X) < 0.5) then
      repeat
        X := X * 2;
        Dec(Exponent);
      until (Abs(X) >= 0.5)
    else
      while (Abs(X) >= 1) do
      begin
        X := X / 2;
        Inc(Exponent);
      end;
  Mantissa := X;
{$ELSE}
asm
  FLD     X
  PUSH    EAX
  MOV     dword ptr [edx], 0    { if X = 0, return 0 }

  FTST
  FSTSW   AX
  FWAIT
  SAHF
  JZ      @@Done

  FXTRACT                 // ST(1) = exponent, (pushed) ST = fraction
  FXCH

  // The FXTRACT instruction normalizes the fraction 1 bit higher than
  // wanted for the definition of frexp() so we need to tweak the result
  // by scaling the fraction down and incrementing the exponent.

  FISTP   dword ptr [edx]
  FLD1
  FCHS
  FXCH
  FSCALE                  // scale fraction
  INC     dword ptr [edx] // exponent biased to match
  FSTP ST(1)              // discard -1, leave fraction as TOS

@@Done:
  POP     EAX
  FSTP    tbyte ptr [eax]
  FWAIT
  {$ENDIF}
end;

function Ldexp(X: Extended; const P: Integer): Extended;
{$IFDEF GLS_NO_ASM}
begin
  Ldexp := X * Intpower(2.0, P);
{$ELSE}
{ Result := X * (2^P) }
asm
  PUSH    EAX
  FILD    dword ptr [ESP]
  FLD     X
  FSCALE
  POP     EAX
  FSTP    ST(1)
  FWAIT
  {$ENDIF}
end;

// standard conversion from float pixels to rgbe pixels
procedure Float2rgbe(var RGBE: TVector4b; const Red, Green, Blue: Single);
var
  V, M: Extended;
  E: Integer;
begin
  V := Red;
  if (Green > V) then
    V := Green;
  if (Blue > V) then
    V := Blue;
  if (V < 1E-32) then
  begin
    RGBE.V[0] := 0;
    RGBE.V[1] := 0;
    RGBE.V[2] := 0;
    RGBE.V[3] := 0;
  end
  else
  begin
    FrExp(V, M, E);
    M := M * 256 / V;
    RGBE.V[0] := Floor(Red * V);
    RGBE.V[1] := Floor(Green * V);
    RGBE.V[2] := Floor(Blue * V);
    RGBE.V[3] := Floor(E + 128);
  end;
end;

// standard conversion from rgbe to float pixels
// note: Ward uses ldexp(col+0.5,exp-(128+8)).  However we wanted pixels
// in the range [0,1] to map back into the range [0,1].
procedure Rgbe2float(var Red, Green, Blue: Single; const RGBE: TVector4b);
var
  F: Single;
begin
  if RGBE.V[3] <> 0 then // nonzero pixel
  begin
    F := Ldexp(1.0, RGBE.V[3] - (128 + 8));
    Red := RGBE.V[0] * F;
    Green := RGBE.V[1] * F;
    Blue := RGBE.V[2] * F;
  end
  else
  begin
    Red := 0;
    Green := 0;
    Blue := 0;
  end;
end;

procedure LoadRLEpixels(Stream: TStream; Dst: PSingle;
  Scanline_width, Num_scanlines: Integer);
var
  RgbeTemp: TVector4b;
  Buf: TVector2b;
  Rf, Gf, Bf: Single;
  Scanline_buffer: PByteArray;
  Ptr, Ptr_end: PByte;
  I: Integer;
  Count: Cardinal;
begin
  if (Scanline_width < 8) or (Scanline_width > $7FFF) then
  begin
    // run length encoding is not allowed so read flat
    LoadRGBEPixels(Stream, Dst, Scanline_width * Num_scanlines);
    Exit;
  end;

  Scanline_buffer := nil;
  while Num_scanlines > 0 do
  begin
    Stream.Read(RgbeTemp, SizeOf(TVector4b));
    if (RgbeTemp.V[0] <> 2) or (RgbeTemp.V[1] <> 2) or
      (RgbeTemp.V[2] and $80 <> 0) then
    begin
      // this file is not run length encoded
      Rgbe2float(Rf, Gf, Bf, RgbeTemp);
      Dst^ := Rf;
      Inc(Dst);
      Dst^ := Gf;
      Inc(Dst);
      Dst^ := Bf;
      Inc(Dst);
      if Assigned(Scanline_buffer) then
        FreeMem(Scanline_buffer);
      LoadRGBEpixels(Stream, Dst, Scanline_width * Num_scanlines - 1);
      Exit;
    end;
    if ((Integer(RgbeTemp.V[2]) shl 8) or RgbeTemp.V[3]) <> Scanline_width
    then
    begin
      if Assigned(Scanline_buffer) then
        FreeMem(Scanline_buffer);
      raise ERGBEexception.Create('Wrong scanline width.');
    end;

    if not Assigned(Scanline_buffer) then
      ReallocMem(Scanline_buffer, 4 * Scanline_width);

    Ptr := PByte(Scanline_buffer);
    // read each of the four channels for the scanline into the buffer
    for I := 0 to 3 do
    begin
      Ptr_end := @Scanline_buffer[(I + 1) * Scanline_width];
      while PtrUInt(Ptr) < PtrUInt(Ptr_end) do
      begin
        Stream.Read(Buf, SizeOf(TVector2b));
        if Buf.V[0] > 128 then
        begin // a run of the same value
          Count := Buf.V[0] - 128;
          if (Count = 0) or (Count > PtrUInt(Ptr_end) - PtrUInt(Ptr)) then
          begin
            FreeMem(Scanline_buffer);
            raise ERGBEexception.Create('Bad HDR scanline data.');
          end;
          while Count > 0 do
          begin
            Ptr^ := Buf.V[1];
            Dec(Count);
            Inc(Ptr);
          end;
        end
        else
        begin // a non-run
          Count := Buf.V[0];
          if (Count = 0) or (Count > PtrUInt(Ptr_end) - PtrUInt(Ptr)) then
          begin
            FreeMem(Scanline_buffer);
            raise ERGBEexception.Create('Bad HDR scanline data.');
          end;
          Ptr^ := Buf.V[1];
          Dec(Count);
          Inc(Ptr);
          if Count > 0 then
            Stream.Read(Ptr^, Count);
          Inc(Ptr, Count);
        end;
      end;
    end;

    // now convert data from buffer into floats
    for I := 0 to Scanline_width - 1 do
    begin
      RgbeTemp.V[0] := Scanline_buffer[I];
      RgbeTemp.V[1] := Scanline_buffer[I + Scanline_width];
      RgbeTemp.V[2] := Scanline_buffer[I + 2 * Scanline_width];
      RgbeTemp.V[3] := Scanline_buffer[I + 3 * Scanline_width];
      Rgbe2float(Rf, Gf, Bf, RgbeTemp);
      Dst^ := Rf;
      Inc(Dst);
      Dst^ := Gf;
      Inc(Dst);
      Dst^ := Bf;
      Inc(Dst);
    end;
    Dec(Num_scanlines);
  end;
  if Assigned(Scanline_buffer) then
    FreeMem(Scanline_buffer);
end;

procedure LoadRGBEpixels(Stream: TStream; Dst: PSingle; Numpixels: Integer);
var
  RgbeTemp: TVector4b;
  Rf, Gf, Bf: Single;
begin
  while Numpixels > 0 do
  begin
    Stream.Read(RgbeTemp, SizeOf(TVector4b));
    Rgbe2float(Rf, Gf, Bf, RgbeTemp);
    Dst^ := Rf;
    Inc(Dst);
    Dst^ := Gf;
    Inc(Dst);
    Dst^ := Bf;
    Inc(Dst);
    Dec(Numpixels);
  end;
end;

end.
