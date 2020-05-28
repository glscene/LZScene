//
// This unit is part of the GLScene Project, http://glscene.org
//
{

  History :  
         23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
         31/05/10 - Yar - Fixes for Linux x64
         08/05/10 - Yar - Removed check for residency in AssignFromTexture
         22/04/10 - Yar - Fixes after GLState revision
         16/03/10 - Yar - Improved FPC compatibility
         05/03/10 - Yar - Creation
    
}
unit GLFilePNG;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  GLCrossPlatform, OpenGLTokens, GLContext, GLGraphics,
  GLTextureFormat, GLApplicationFileIO;

type

  TGLPNGImage = class(TGLBaseImage)
  private
  public
    class function Capabilities: TGLDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    { Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLuint;
      textureTarget: TGLTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); reintroduce;
  end;

implementation

uses
  LIBPNG;

resourcestring
  sLIBPNGerror = 'LIBPNG error';

// ------------------
// ------------------ TGLPNGImage ------------------
// ------------------

 
//

procedure TGLPNGImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
      ResourceName := filename;
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
end;

// SaveToFile
//

procedure TGLPNGImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

// LoadFromStream
//

procedure TGLPNGImage.LoadFromStream(stream: TStream);
var
  sig: array[0..7] of Byte;
  png_ptr: png_structp;
  info_ptr: png_infop;
  colorType, bitDepth: Integer;
  rowBytes: Cardinal;
  rowPointers: array of PGLUbyte;
  ii: Integer;
  use16: Boolean;
begin
  stream.Read(sig, 8);

  if _png_sig_cmp(@sig, 0, 8) <> 0 then
    raise EInvalidRasterFile.Create('Invalid PNG file');

  png_ptr := _png_create_read_struct(ZLIB_VERSION, nil, pngErrorFn, pngWarnFn);

  if not Assigned(png_ptr) then
    raise EInvalidRasterFile.Create(sLIBPNGerror);

  info_ptr := _png_create_info_struct(png_ptr);

  if not Assigned(png_ptr) then
  begin
    _png_destroy_read_struct(@png_ptr, nil, nil);
    raise EInvalidRasterFile.Create(sLIBPNGerror);
  end;

  UnMipmap;

  try
    { Need to override the standard I/O methods since libPNG
       may be linked against a different run-time }
    _png_set_read_fn(png_ptr, stream, pngReadFn);
    // skip the sig bytes
    _png_set_sig_bytes(png_ptr, 8);

    // automagically read everything to the image data
    _png_read_info(png_ptr, info_ptr);
    FLOD[0].Width := _png_get_image_width(png_ptr, info_ptr);
    FLOD[0].Height := _png_get_image_height(png_ptr, info_ptr);
    // using the convention of depth = 0 for 2D images
    FLOD[0].Depth := 0;

    colorType := _png_get_color_type(png_ptr, info_ptr);
    bitDepth :=  _png_get_bit_depth(png_ptr, info_ptr);
    { Setup the read transforms
       expand palette images to RGB and low-bit-depth grayscale images to 8 bits
       convert transparency chunks to full alpha channel }
    if colorType = PNG_COLOR_TYPE_PALETTE then
      _png_set_palette_to_rgb(png_ptr);
    if (colorType = PNG_COLOR_TYPE_GRAY) and (bitDepth < 8) then
      _png_set_gray_1_2_4_to_8(png_ptr);


    if _png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) <> 0 then
      _png_set_tRNS_to_alpha(png_ptr);
    // now configure for reading, and allocate the memory
    _png_read_update_info(png_ptr, info_ptr);

    rowBytes := _png_get_rowbytes(png_ptr, info_ptr);

    UpdateLevelsInfo;
    ReallocMem(fData, rowBytes * Cardinal(GetHeight));

    SetLength(rowPointers, GetHeight);

    // set up the row pointers
    for ii := 0 to FLOD[0].Height - 1 do
      rowPointers[ii] := PGLUbyte(PtrUInt(fData) +
      Cardinal(FLOD[0].Height - 1 - ii) * rowBytes);

    // read the image
    _png_read_image(png_ptr, @rowPointers[0]);

    use16 := bitDepth > 8;

    if use16 then
      fDataType := GL_UNSIGNED_SHORT
    else
      fDataType := GL_UNSIGNED_BYTE;

    case
    _png_get_channels(png_ptr, info_ptr)
    of
      1:
        begin
          fColorFormat := GL_LUMINANCE;
          if use16 then
          begin
            fInternalFormat := tfLUMINANCE16;
            fElementSize := 2;
          end
          else
          begin
            fInternalFormat := tfLUMINANCE8;
            fElementSize := 1;
          end;
        end;
      2:
        begin
          fColorFormat := GL_LUMINANCE_ALPHA;
          if use16 then
          begin
            fInternalFormat := tfLUMINANCE16_ALPHA16;
            fElementSize := 4;
          end
          else
          begin
            fInternalFormat := tfLUMINANCE8_ALPHA8;
            fElementSize := 2;
          end;
        end;
      3:
        begin
          fColorFormat := GL_RGB;
          if use16 then
          begin
            fInternalFormat := tfR16G16B16;
            fElementSize := 6;
          end
          else
          begin
            fInternalFormat := tfRGB8;
            fElementSize := 3;
          end;
        end;
      4:
        begin
          fColorFormat := GL_RGBA;
          if use16 then
          begin
            fInternalFormat := tfR16G16B16A16;
            fElementSize := 8;
          end
          else
          begin
            fInternalFormat := tfRGBA8;
            fElementSize := 4;
          end;
        end;
    end;

    fCubeMap := false;
    fTextureArray := false;

    _png_read_end(png_ptr, nil);
  finally
    _png_destroy_read_struct(@png_ptr, @info_ptr, nil);
  end;
end;

// SaveToStream
//

procedure TGLPNGImage.SaveToStream(stream: TStream);
var
  png_ptr: png_structp;
  info_ptr: png_infop;
  bit_depth, color_type: Integer;
  rowBytes: Cardinal;
  canSave: Boolean;
  rowPointers: array of PGLUbyte;
  ii: Integer;
begin
  png_ptr := _png_create_write_struct(ZLIB_VERSION, nil, pngErrorFn, pngWarnFn);

  if not Assigned(png_ptr) then
    raise EInvalidRasterFile.Create(sLIBPNGerror);

  info_ptr := _png_create_info_struct(png_ptr);
  if not Assigned(png_ptr) then
  begin
    _png_destroy_write_struct(@png_ptr, nil);
    raise EInvalidRasterFile.Create(sLIBPNGerror);
  end;

  try
    { Need to override the standard I/O methods since
      libPNG may be linked against a different run-time }
    _png_set_write_fn(png_ptr, stream, pngWriteFn, nil);
    bit_depth := fElementSize * 8;
    color_type := PNG_COLOR_TYPE_GRAY;
    rowBytes := GetWidth * fElementSize;
    canSave := true;
    case fDataType of
      GL_UNSIGNED_BYTE: bit_depth := 8;
      GL_UNSIGNED_SHORT: bit_depth := 16;
    else
      canSave := false;
    end;

    case fColorFormat of
      GL_LUMINANCE: color_type := PNG_COLOR_TYPE_GRAY;
      GL_LUMINANCE_ALPHA: color_type := PNG_COLOR_TYPE_GRAY_ALPHA;
      GL_RGB: color_type := PNG_COLOR_TYPE_RGB;
      GL_RGBA: color_type := PNG_COLOR_TYPE_RGB_ALPHA;
    else
      canSave := false;
    end;

    if not canSave then
      raise
        EInvalidRasterFile.Create('These image format do not match the PNG format specification.');

    _png_set_IHDR(png_ptr, info_ptr, GetWidth, GetHeight, bit_depth, color_type,
      PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
      PNG_FILTER_TYPE_DEFAULT);
    // write the file header information
    _png_write_info(png_ptr, info_ptr);
    SetLength(rowPointers, GetHeight);

    // set up the row pointers
    for ii := 0 to GetHeight - 1 do
      rowPointers[ii] := PGLUbyte(PtrUInt(fData) +
      Cardinal(GetHeight - 1 - ii) * rowBytes);

    _png_write_image(png_ptr, @rowPointers[0]);
    _png_write_end(png_ptr, info_ptr);
  finally
    _png_destroy_write_struct(@png_ptr, @info_ptr);
  end;
end;

// AssignFromTexture
//

procedure TGLPNGImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLuint;
  textureTarget: TGLTextureTarget;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat: Cardinal;
  residentFormat: TGLInternalFormat;
  glTarget: TGLEnum;
begin
  if not ((textureTarget = ttTexture2D)
    or (textureTarget = ttTextureRect)) then
    Exit;

  oldContext := CurrentGLContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;
  glTarget := DecodeGLTextureTarget(textureTarget);

  try
    textureContext.GLStates.TextureBinding[0, textureTarget] := textureHandle;
    fLevelCount := 0;
    fCubeMap := false;
    fTextureArray := false;
    // Check level existence
    GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_INTERNAL_FORMAT,
      @texFormat);
    if texFormat > 1 then
    begin
      GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
      GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT, @FLOD[0].Height);
      FLOD[0].Depth := 0;
      residentFormat := OpenGLFormatToInternalFormat(texFormat);
      if CurrentFormat then
        fInternalFormat := residentFormat
      else
        fInternalFormat := intFormat;
      FindCompatibleDataFormat(fInternalFormat, fColorFormat, fDataType);
      Inc(fLevelCount);
    end;
    if fLevelCount > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(FData, DataSize);
      GL.GetTexImage(glTarget, 0, fColorFormat, fDataType, fData);
    end
    else
      fLevelCount := 1;
    GL.CheckError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;

// Capabilities
//

class function TGLPNGImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

initialization
  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('png', 'Portable Network Graphic', TGLPNGImage);

end.
