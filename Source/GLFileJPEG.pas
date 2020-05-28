//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   History :  
       23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
       29/06/10 - Yar - Improved FPC compatibility
       29/04/10 - Yar - Bugfixed loading of fliped image (thanks mif)
       27/02/10 - Yar - Creation
   
}
unit GLFileJPEG;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  GLCrossPlatform, OpenGLTokens, GLContext, GLGraphics, GLTextureFormat,
  GLApplicationFileIO, GLSLog;

type

  TGLJPEGImage = class(TGLBaseImage)
  private
    FAbortLoading: boolean;
    FDivScale: longword;
    FDither: boolean;
    FSmoothing: boolean;
    FProgressiveEncoding: boolean;
    procedure SetSmoothing(const AValue: boolean);
  public
    constructor Create; override;
    class function Capabilities: TGLDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    { Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLuint;
      textureTarget: TGLTextureTarget;
      const CurrentFormat: boolean;
      const intFormat: TGLInternalFormat); reintroduce;

    property DivScale: longword read FDivScale write FDivScale;
    property Dither: boolean read FDither write FDither;
    property Smoothing: boolean read FSmoothing write SetSmoothing;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
  end;

implementation

uses
  FPReadJPEG,
  fpimage,
  jmorecfg,
  jpeglib,
  jerror,
  jdeferr,
  jdmarker,
  jdmaster,
  jdapimin,
  jdapistd,
  jcparam,
  jcapimin,
  jcapistd,
  jcomapi,
  jdatasrc,
  jmemmgr,

  GLVectorGeometry;


// ------------------
// ------------------ TGLJPEGImage ------------------
// ------------------

constructor TGLJPEGImage.Create;
begin
  inherited;
  FAbortLoading := False;
  FDivScale := 1;
  FDither := False;
end;

 


procedure TGLJPEGImage.LoadFromFile(const filename: string);
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


procedure TGLJPEGImage.SaveToFile(const filename: string);
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


procedure JPEGError(CurInfo: j_common_ptr);
begin
  if CurInfo = nil then
    exit;
  raise Exception.CreateFmt('JPEG error', [CurInfo^.err^.msg_code]);
end;

procedure EmitMessage(CurInfo: j_common_ptr; msg_level: integer);
begin
  if CurInfo = nil then
    exit;
  if msg_level = 0 then
  ;
end;

procedure OutputMessage(CurInfo: j_common_ptr);
begin
  if CurInfo = nil then
    exit;
end;

procedure FormatMessage(CurInfo: j_common_ptr; var buffer: string);
begin
  if CurInfo = nil then
    exit;
  GLSLogger.LogInfo(buffer);
end;

procedure ResetErrorMgr(CurInfo: j_common_ptr);
begin
  if CurInfo = nil then
    exit;
  CurInfo^.err^.num_warnings := 0;
  CurInfo^.err^.msg_code := 0;
end;

procedure ProgressCallback(CurInfo: j_common_ptr);
begin
  if CurInfo = nil then
    exit;
  // ToDo
end;

var
  jpeg_std_error: jpeg_error_mgr;

procedure TGLJPEGImage.LoadFromStream(stream: TStream);
var
  MemStream: TMemoryStream;
  vInfo: jpeg_decompress_struct;
  vError: jpeg_error_mgr;
  jc: TFPJPEGProgressManager;

  procedure SetSource;
  begin
    MemStream.Position := 0;
    jpeg_stdio_src(@vInfo, @MemStream);
  end;

  procedure ReadHeader;
  begin
    UnMipmap;
    jpeg_read_header(@vInfo, True);
    FLOD[0].Width := vInfo.image_width;
    FLOD[0].Height := vInfo.image_height;
    FLOD[0].Depth := 0;
    if vInfo.jpeg_color_space = JCS_CMYK then
    begin
      FColorFormat := GL_RGBA;
      FInternalFormat := tfRGBA8;
      FElementSize := 4;
    end
    else
    if vInfo.out_color_space = JCS_GRAYSCALE then
    begin
      FColorFormat := GL_LUMINANCE;
      FInternalFormat := tfLUMINANCE8;
      FElementSize := 1;
    end
    else
    begin
      FColorFormat := GL_RGB;
      FInternalFormat := tfRGB8;
      FElementSize := 3;
    end;
    FDataType := GL_UNSIGNED_BYTE;
    FCubeMap := False;
    FTextureArray := False;
    ReallocMem(FData, DataSize);
    FProgressiveEncoding := jpeg_has_multiple_scans(@vInfo);
  end;

  procedure InitReadingPixels;
  begin
    vInfo.scale_num := 1;
    vInfo.scale_denom := 1;
    vInfo.do_block_smoothing := FSmoothing;

    if vInfo.out_color_space = JCS_GRAYSCALE then
    begin
      vInfo.quantize_colors := True;
      vInfo.desired_number_of_colors := 236;
    end;

    if FProgressiveEncoding then
    begin
      vInfo.enable_2pass_quant := vInfo.two_pass_quantize;
      vInfo.buffered_image := True;
    end;
  end;

  function CorrectCMYK(const C: TFPColor): TFPColor;
  var
    MinColor: word;
  begin
    if C.red < C.green then
      MinColor := C.red
    else
      MinColor := C.green;
    if C.blue < MinColor then
      MinColor := C.blue;
    if MinColor + C.alpha > $FF then
      MinColor := $FF - C.alpha;
    Result.red := (C.red - MinColor) shl 8;
    Result.green := (C.green - MinColor) shl 8;
    Result.blue := (C.blue - MinColor) shl 8;
    Result.alpha := alphaOpaque;
  end;

  procedure ReadPixels;
  var
    SampArray: JSAMPARRAY;
    SampRow: JSAMPROW;
    Color: TFPColor;
    LinesRead: Cardinal;
    y: Integer;
    Status, Scan: integer;
    ReturnValue, RestartLoop: boolean;

    procedure OutputScanLines();
    var
      x: integer;
      pPixel: PByte;
    begin
      Color.Alpha := alphaOpaque;
      y := FLOD[0].Height - 1;
      while (vInfo.output_scanline < vInfo.output_height) do
      begin
        // read one line per call
        LinesRead := jpeg_read_scanlines(@vInfo, SampArray, 1);
        if LinesRead < 1 then
        begin
          ReturnValue := False;
          break;
        end;
        pPixel := @PByteArray(FData)[y*FLOD[0].Width*FElementSize];

        if vInfo.jpeg_color_space = JCS_CMYK then
        begin
          for x := 0 to vInfo.output_width - 1 do
          begin
            Color.Red := SampRow^[x * 4 + 0];
            Color.Green := SampRow^[x * 4 + 1];
            Color.Blue := SampRow^[x * 4 + 2];
            Color.alpha := SampRow^[x * 4 + 3];
            Color := CorrectCMYK(Color);
            pPixel^:= Color.red; Inc(pPixel);
            pPixel^:= Color.green; Inc(pPixel);
            pPixel^:= Color.blue; Inc(pPixel);
            pPixel^:= Color.alpha; Inc(pPixel);
          end;
        end
        else // RGB or LUMINANCE
          Move(SampRow^[0], pPixel^, FElementSize * vInfo.output_width);

        Dec(y);
      end;
    end;

  begin
    InitReadingPixels;

    jpeg_start_decompress(@vInfo);

    GetMem(SampArray, SizeOf(JSAMPROW));
    GetMem(SampRow, vInfo.output_width * vInfo.output_components);
    SampArray^[0] := SampRow;
    try
      case FProgressiveEncoding of
        False:
          begin
            ReturnValue:=true;
            OutputScanLines();
            if vInfo.buffered_image then jpeg_finish_output(@vInfo);
          end;
        True:
          begin
            while true do
            begin
              (* The RestartLoop variable drops a placeholder for suspension
                 mode, or partial jpeg decode, return and continue. In case
                 of support this suspension, the RestartLoop:=True should be
                 changed by an Exit and in the routine enter detects that it
                 is being called from a suspended state to not
                 reinitialize some buffer *)
              RestartLoop:=false;
              repeat
                status := jpeg_consume_input(@vInfo);
              until (status=JPEG_SUSPENDED) or (status=JPEG_REACHED_EOI);
              ReturnValue:=true;
              if vInfo.output_scanline = 0 then
              begin
                Scan := vInfo.input_scan_number;
                (* if we haven't displayed anything yet (output_scan_number==0)
                  and we have enough data for a complete scan, force output
                  of the last full scan *)
                if (vInfo.output_scan_number = 0) and (Scan > 1) and
                  (status <> JPEG_REACHED_EOI) then Dec(Scan);

                if not jpeg_start_output(@vInfo, Scan) then
                begin
                  RestartLoop:=true; (* I/O suspension *)
                end;
              end;

              if not RestartLoop then
              begin
                if (vInfo.output_scanline = $ffffff) then
                  vInfo.output_scanline := 0;

                OutputScanLines();

                if ReturnValue=false then begin
                  if (vInfo.output_scanline = 0) then
                  begin
                     (* didn't manage to read any lines - flag so we don't call
                        jpeg_start_output() multiple times for the same scan *)
                     vInfo.output_scanline := $ffffff;
                  end;
                  RestartLoop:=true; (* I/O suspension *)
                end;

                if not RestartLoop then
                begin
                  if (vInfo.output_scanline = vInfo.output_height) then
                  begin
                    if not jpeg_finish_output(@vInfo) then
                    begin
                      RestartLoop:=true; (* I/O suspension *)
                    end;

                    if not RestartLoop then
                    begin
                      if (jpeg_input_complete(@vInfo) and
                         (vInfo.input_scan_number = vInfo.output_scan_number)) then
                        break;

                      vInfo.output_scanline := 0;
                    end;
                  end;
                end;
              end;
              if RestartLoop then
                break;
            end;
          end;
      end;
    finally
      FreeMem(SampRow);
      FreeMem(SampArray);
    end;

    jpeg_finish_decompress(@vInfo);
  end;

begin
  MemStream := nil;
  FillChar(vInfo, SizeOf(vInfo), $00);
  try

    if stream is TMemoryStream then
      MemStream := TMemoryStream(stream)
    else
    begin
      MemStream := TMemoryStream.Create;
      MemStream.CopyFrom(stream, stream.Size - stream.Position);
      MemStream.Position := 0;
    end;

    if MemStream.Size > 0 then
    begin
      vError := jpeg_std_error;
      vInfo.err := @vError;
      jpeg_CreateDecompress(@vInfo, JPEG_LIB_VERSION, SizeOf(vInfo));
      try
        jc.pub.progress_monitor := @ProgressCallback;
        jc.instance := Self;
        vInfo.progress := @jc.pub;
        SetSource;
        ReadHeader;
        ReadPixels;
      finally
        jpeg_Destroy_Decompress(@vInfo);
      end;
    end;
  finally
    if Assigned(MemStream) and (MemStream <> stream) then
      MemStream.Free;
  end;
end;



procedure TGLJPEGImage.SaveToStream(stream: TStream);
begin

end;

// AssignFromTexture


procedure TGLJPEGImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLuint; textureTarget: TGLTextureTarget;
  const CurrentFormat: boolean; const intFormat: TGLInternalFormat);
begin

end;

procedure TGLJPEGImage.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing <> AValue then
    FSmoothing := AValue;
end;

// Capabilities


class function TGLJPEGImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead {, dfcWrite}];
end;

initialization


  with jpeg_std_error do
  begin
    error_exit := @JPEGError;
    emit_message := @EmitMessage;
    output_message := @OutputMessage;
    format_message := @FormatMessage;
    reset_error_mgr := @ResetErrorMgr;
  end;


  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('jpg', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
  RegisterRasterFormat('jpeg', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
  RegisterRasterFormat('jpe', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
end.

