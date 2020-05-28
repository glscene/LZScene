//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    Good for preview picture in OpenDialog,
    so you may include both HDRImage (preview) and GLFileHDR (loading)

       23/10/10 - Yar - Removed PBuffer    
       23/08/10 - Yar - Changes after PBuffer upgrade
       21/03/10 - Yar - Added Linux support
                           (thanks to Rustam Asmandiarov aka Predator)
       24/01/10 - Yar - Improved FPC compatibility
       21/01/10 - Yar - Creation
    
}

unit HDRImage;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}Windows,
{$ENDIF}Classes,
  SysUtils,
  GLCrossPlatform,
  GLVectorGeometry,
  GLGraphics,
  OpenGLTokens;

type

  THDRImage = class(TGLBitmap)
  public
     
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

implementation

uses
graphtype,
  GLFileHDR,
  GLTextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

// LoadFromStream
//

procedure THDRImage.LoadFromStream(stream: TStream);
var
  FullHDR: TGLHDRImage;

  RIMG: TRawImage;

begin
  FullHDR := TGLHDRImage.Create;
  try
    FullHDR.LoadFromStream(stream);
  except
    FullHDR.Free;
    raise;
  end;

  FullHDR.Narrow;

  Width := FullHDR.LevelWidth[0];
  Height := FullHDR.LevelHeight[0];
  Transparent := false;
  PixelFormat := glpf32bit;


  RIMG.Init;
  rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
  rimg.Description.RedShift := 16;
  rimg.Description.BlueShift := 0;
  rimg.Description.LineOrder := riloBottomToTop;
  RIMG.DataSize := Width * Height * 4;
  rimg.Data := PByte(FullHDR.Data);
  LoadFromRawImage(rimg, false);

  FullHDR.Free;
end;

// SaveToStream
//

procedure THDRImage.SaveToStream(stream: TStream);
begin
  Assert(False, 'Not supported');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.RegisterFileFormat(
    'HDR', 'High Dynamic Range Image', THDRImage);

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
finalization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.UnregisterGraphicClass(THDRImage);

end.

