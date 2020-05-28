//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    Alternative for DDS unit with more supported formats of flat image:
    Alpha8, Luminance8, R3G3B2, RGB5A1, RGBA4, Alpha8Luminance8, Luminance16, R5G6B5,
    RGB8, R10G10B10A2, RGBA8, RGBA16, R16F, RGBA16F, R32F, RGBA32F, GR16, GR16F, GR32F,
    Compressed RGB S3TC DXT1, Compressed RGBA S3TC DXT1, Compressed RGBA S3TC DXT3,
    Compressed RGBA S3TC DXT5
    But it down color to RGBA8 because becomes to TGLBitmap
    Good for preview picture in OpenDialog,
    so you may include both DDSImage (preview) and GLFileDDS (loading)

  History :  
         23/10/10 - Yar - Removed PBuffer
         23/08/10 - Yar - Changes after PBuffer upgrade
         20/05/10 - Yar - Fixes for Linux x64
         21/03/10 - Yar - Added Linux support
                             (thanks to Rustam Asmandiarov aka Predator)
         24/01/10 - Yar - Improved FPC compatibility
         21/01/10 - Yar - Creation
    
}

unit DDSImage;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}Windows,
{$ENDIF}
  Classes,
  SysUtils,
  GLCrossPlatform,
  GLVectorGeometry,
  GLGraphics,
  OpenGLTokens,
  GLContext;

type

  TDDSImage = class(TGLBitmap)
  public
     
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

  EDDSException = class(Exception);

implementation

uses
  graphtype,
  LCLType,
  DXTC,
  GLFileDDS,
  GLTextureFormat;

// ------------------
// ------------------ TDDSImage ------------------
// ------------------

// LoadFromStream

procedure TDDSImage.LoadFromStream(stream: TStream);
var
  FullDDS: TGLDDSImage;
  bCubeMap: Boolean;
  RIMG: TRawImage;
begin
  FullDDS := TGLDDSImage.Create;
  try
    FullDDS.LoadFromStream(stream);
  except
    FullDDS.Free;
    raise;
  end;

  bCubeMap := FullDDS.CubeMap;
  FullDDS.Narrow;

  PixelFormat := glpf32bit;
  Transparent := True;
  Width := FullDDS.LevelWidth[0];
  Height := FullDDS.LevelHeight[0];


  RIMG.Init;
  rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
  rimg.Description.RedShift := 16;
  rimg.Description.BlueShift := 0;
  if bCubeMap then
    rimg.Description.LineOrder := riloTopToBottom
  else
    rimg.Description.LineOrder := riloBottomToTop;
  RIMG.DataSize := Width * Height * 4;
  rimg.Data := PByte(FullDDS.Data);
  LoadFromRawImage(rimg, false);

  FullDDS.Free;
end;

// SaveToStream

procedure TDDSImage.SaveToStream(stream: TStream);
const
  Magic: array[0..3] of AnsiChar = 'DDS ';
var
  header: TDDSHeader;
  rowSize: integer;
{$IFNDEF FPC}
  i: Integer;
{$ENDIF}
begin
  FillChar(header, SizeOf(TDDSHeader), 0);
  header.magic := cardinal(Magic);
  with header.SurfaceFormat do
  begin
    dwSize := sizeof(TDDSURFACEDESC2);
    dwFlags := DDSD_CAPS + DDSD_PIXELFORMAT + DDSD_WIDTH + DDSD_HEIGHT + DDSD_PITCH;
    dwWidth := Width;
    dwHeight := Height;
    ddpf.dwSize := sizeof(TDDPIXELFORMAT);
    case PixelFormat of
{$IFDEF MSWINDOWS}
      glpf24bit:
        begin
          ddpf.dwFlags := DDPF_RGB;
          ddpf.dwRGBBitCount := 24;
          ddpf.dwRBitMask := $00FF0000;
          ddpf.dwGBitMask := $0000FF00;
          ddpf.dwBBitMask := $000000FF;
        end;
{$ENDIF}
      glpf32bit:
        begin
          ddpf.dwFlags := DDPF_RGB;
          ddpf.dwRGBBitCount := 32;
          ddpf.dwRBitMask := $00FF0000;
          ddpf.dwGBitMask := $0000FF00;
          ddpf.dwBBitMask := $000000FF;
          if Transparent then
          begin
            ddpf.dwFlags := ddpf.dwFlags + DDPF_ALPHAPIXELS;
            ddpf.dwRGBAlphaBitMask := $FF000000;
          end;
        end;
    else
      raise EDDSException.Create('Unsupported pixel format format');
    end;
    rowSize := (ddpf.dwRGBBitCount div 8) * dwWidth;
    dwPitchOrLinearSize := dwHeight * cardinal(rowSize);
    dwCaps := DDSCAPS_TEXTURE;
    stream.Write(header, SizeOf(TDDSHeader));
{$IFNDEF FPC}
    for i := 0 to Height - 1 do
      stream.Write(ScanLine[i]^, rowSize);
{$ELSE}
    stream.Write(RawImage.Data^, Width * Height * header.SurfaceFormat.ddpf.dwRGBBitCount div 4);
{$ENDIF}
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.RegisterFileFormat(
    'dds', 'Microsoft DirectDraw Surface', TDDSImage);

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
finalization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.UnregisterGraphicClass(TDDSImage);

end.

