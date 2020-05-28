//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Simple TGA formats supports for Delphi. 
   Currently supports only 24 and 32 bits RGB formats (uncompressed
   and RLE compressed).

   Based on David McDuffee's document from www.wotsit.org

	 History :  
            07/03/11 - Yar - Removed LazTGA, added workaround of ScanLine for Lazarus
            20/04/10 - Yar - Removed registration for FPC (thanks to Rustam Asmandiarov aka Predator) 
	    07/01/10 - DaStr - TTGAImage is now replaced by LazTGA.TTGAImage
                              in Lazarus (thanks Predator)   
	    08/07/04 - LR - Uses of Graphics replaced by GLCrossPlatform for Linux
	    21/11/02 - Egg - Creation
	 
}
unit TGA;

interface

{$I GLScene.inc}

uses 
  Classes, 
  SysUtils, 
  GLCrossPlatform;

type

   { TGA image load/save capable class for Delphi.
      TGA formats supported : 24 and 32 bits uncompressed or RLE compressed,
      saves only to uncompressed TGA. }
        TTGAImage = class (TGLBitmap)
	       

	   public
	       
	      constructor Create; override;
         destructor Destroy; override;

         procedure LoadFromStream(stream : TStream); override;
         procedure SaveToStream(stream : TStream); override;
	end;

   ETGAException = class (Exception)
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  GLGraphics,GraphType, LCLType ;

type

   // TTGAHeader
   //
   TTGAHeader = packed record
      IDLength          : Byte;
      ColorMapType      : Byte;
      ImageType         : Byte;
      ColorMapOrigin    : Word;
      ColorMapLength    : Word;
      ColorMapEntrySize : Byte;
      XOrigin           : Word;
      YOrigin           : Word;
      Width             : Word;
      Height            : Word;
      PixelSize         : Byte;
      ImageDescriptor   : Byte;
  end;

// ReadAndUnPackRLETGA24
//
procedure ReadAndUnPackRLETGA24(stream : TStream; destBuf : PAnsiChar; totalSize : Integer);
type
   TRGB24 = packed record
      r, g, b : Byte;
   end;
   PRGB24 = ^TRGB24;
var
   n : Integer;
   color : TRGB24;
   bufEnd : PAnsiChar;
   b : Byte;
begin
   bufEnd:=@destBuf[totalSize];
   while destBuf<bufEnd do 
   begin
      stream.Read(b, 1);
      if b>=128 then 
	  begin
         // repetition packet
         stream.Read(color, 3);
         b:=(b and 127)+1;
         while b>0 do 
		 begin
            PRGB24(destBuf)^:=color;
            Inc(destBuf, 3);
            Dec(b);
         end;
      end 
	  else 
	  begin
         n:=((b and 127)+1)*3;
         stream.Read(destBuf^, n);
         Inc(destBuf, n);
      end;
   end;
end;

// ReadAndUnPackRLETGA32
//
procedure ReadAndUnPackRLETGA32(stream : TStream; destBuf : PAnsiChar; totalSize : Integer);
type
   TRGB32 = packed record
      r, g, b, a : Byte;
   end;
   PRGB32 = ^TRGB32;
var
   n : Integer;
   color : TRGB32;
   bufEnd : PAnsiChar;
   b : Byte;
begin
   bufEnd:=@destBuf[totalSize];
   while destBuf<bufEnd do 
   begin
      stream.Read(b, 1);
      if b>=128 then 
	  begin
         // repetition packet
         stream.Read(color, 4);
         b:=(b and 127)+1;
         while b>0 do 
		 begin
            PRGB32(destBuf)^:=color;
            Inc(destBuf, 4);
            Dec(b);
         end;
      end 
	  else 
	  begin
         n:=((b and 127)+1)*4;
         stream.Read(destBuf^, n);
         Inc(destBuf, n);
      end;
   end;
end;

// ------------------
// ------------------ TTGAImage ------------------
// ------------------

// Create
//
constructor TTGAImage.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TTGAImage.Destroy;
begin
	inherited Destroy;
end;

// LoadFromStream
//
procedure TTGAImage.LoadFromStream(stream : TStream);
var
   header : TTGAHeader;
   y, rowSize, bufSize : Integer;
   verticalFlip : Boolean;
   unpackBuf : PAnsiChar;

   rimg: TRawImage;


   function GetLineAddress(ALine: Integer): PByte;
   begin
     Result := PByte(@PGLPixel32Array(rimg.Data)[ALine*Width]);
   end;

begin
   stream.Read(header, Sizeof(TTGAHeader));

   if header.ColorMapType<>0 then
      raise ETGAException.Create('ColorMapped TGA unsupported');

   case header.PixelSize of
      24 : PixelFormat:=glpf24bit;
      32 : PixelFormat:=glpf32bit;
   else
      raise ETGAException.Create('Unsupported TGA ImageType');
   end;

   Width:=header.Width;
   Height:=header.Height;
   rowSize:=(Width*header.PixelSize) div 8;
   verticalFlip:=((header.ImageDescriptor and $20)=0);
   if header.IDLength>0 then
      stream.Seek(header.IDLength, soFromCurrent);


    try
     rimg.Init;
     rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
     rimg.Description.RedShift := 16;
     rimg.Description.BlueShift := 0;
     if verticalFlip then
       rimg.Description.LineOrder := riloTopToBottom
     else
       rimg.Description.LineOrder := riloBottomToTop;
     RIMG.DataSize := Width * Height * 4;
     GetMem(rimg.Data, RIMG.DataSize);



     case header.ImageType of
        0 : begin // empty image, support is useless but easy ;)
           Width:=0;
           Height:=0;
           Abort;
        end;
        2 : begin // uncompressed RGB/RGBA
           if verticalFlip then begin
              for y:=0 to Height-1 do
                 stream.Read(GetLineAddress(Height-y-1)^, rowSize);
           end else begin
              for y:=0 to Height-1 do
                 stream.Read(GetLineAddress(y)^, rowSize);
           end;
        end;
        10 : begin // RLE encoded RGB/RGBA
           bufSize:=Height*rowSize;
           unpackBuf:=GetMemory(bufSize);
           try
              // read & unpack everything
              if header.PixelSize=24 then
                 ReadAndUnPackRLETGA24(stream, unpackBuf, bufSize)
              else ReadAndUnPackRLETGA32(stream, unpackBuf, bufSize);
              // fillup bitmap
              if verticalFlip then begin
                 for y:=0 to Height-1 do begin
                    Move(unPackBuf[y*rowSize], GetLineAddress(Height-y-1)^, rowSize);
                 end;
              end else begin
                 for y:=0 to Height-1 do
                    Move(unPackBuf[y*rowSize], GetLineAddress(y)^, rowSize);
              end;
           finally
              FreeMemory(unpackBuf);
           end;
        end;
     else
        raise ETGAException.Create('Unsupported TGA ImageType '+IntToStr(header.ImageType));
     end;


     LoadFromRawImage(rimg, False);

   finally

     FreeMem(rimg.Data);

   end;
end;

// TTGAImage
//
procedure TTGAImage.SaveToStream(stream : TStream);
var
   header : TTGAHeader;
begin
   // prepare the header, essentially made up from zeroes
   FillChar(header, SizeOf(TTGAHeader), 0);
   header.ImageType:=2;
   header.Width:=Width;
   header.Height:=Height;

   header.PixelSize:=32;
   stream.Write(header, SizeOf(TTGAHeader));
   stream.Write(RawImage.Data^, Width*Height*4);

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
   TGLPicture.RegisterFileFormat('tga', 'Targa', TTGAImage);

finalization

   TGLPicture.UnregisterGraphicClass(TTGAImage);
end.
