//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  History :  
       16/02/11 - PREDATOR - Added support for Mac OS X. Tested on Mac OS X 10.6.5.
       18/05/10 - Yar - Changed to dynamic library loading for FPC (by Predator)
       06/05/10 - Yar - Added to GLScene (contributed by oleg matrozov)
   

******************************************************************************
*  ZLibExApi.pas                                                             *
*                                                                            *
*  copyright (c) 2000-2010 base2 technologies                                *
*  copyright (c) 1995-2002 Borland Software Corporation                      *
*                                                                            *
*  revision history                                                          *
*    2010.04.20  updated to zlib version 1.2.5                               *
*    2010.04.15  updated to zlib version 1.2.4                               *
*    2005.07.25  updated to zlib version 1.2.3                               *
*    2005.01.11  updated to zlib version 1.2.2                               *
*    2004.01.06  updated to zlib version 1.2.1                               *
*    2002.03.15  updated to zlib version 1.1.4                               *
*                                                                            *
*  acknowledgments                                                           *
*    burak kalayci                                                           *
*      2002.03.15  informing me about the zlib 1.1.4 update                  *
*      2004.01.06  informing me about the zlib 1.2.1 update                  *
*                                                                            *
*    vicente sanchez-alarcos                                                 *
*      2005.01.11  informing me about the zlib 1.2.2 update                  *
*                                                                            *
*    mathijs van veluw                                                       *
*      2005.07.25  informing me about the zlib 1.2.3 update                  *
*****************************************************************************}

unit GLSZLibExApi;

interface

{$I ZLibEx.inc}
{$I GLScene.inc}

const
  {** version ids ***********************************************************}



  {** compression methods ***************************************************}

  Z_DEFLATED = 8;

  {** information flags *****************************************************}

  Z_INFO_FLAG_SIZE  = $1;
  Z_INFO_FLAG_CRC   = $2;
  Z_INFO_FLAG_ADLER = $4;

  Z_INFO_NONE       = 0;
  Z_INFO_DEFAULT    = Z_INFO_FLAG_SIZE or Z_INFO_FLAG_CRC;

  {** flush constants *******************************************************}

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;
  Z_TREES         = 6;

  {** return codes **********************************************************}

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  {** compression levels ****************************************************}

  Z_NO_COMPRESSION      =   0;
  Z_BEST_SPEED          =   1;
  Z_BEST_COMPRESSION    =   9;
  Z_DEFAULT_COMPRESSION = (-1);

  {** compression strategies ************************************************}

  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_RLE              = 3;
  Z_FIXED            = 4;
  Z_DEFAULT_STRATEGY = 0;

  {** data types ************************************************************}

  Z_BINARY  = 0;
  Z_ASCII   = 1;
  Z_TEXT    = Z_ASCII;
  Z_UNKNOWN = 2;

  {** return code messages **************************************************}

  _z_errmsg: Array [0..9] of String = (
    'Need dictionary',      // Z_NEED_DICT      (2)
    'Stream end',           // Z_STREAM_END     (1)
    'OK',                   // Z_OK             (0)
    'File error',           // Z_ERRNO          (-1)
    'Stream error',         // Z_STREAM_ERROR   (-2)
    'Data error',           // Z_DATA_ERROR     (-3)
    'Insufficient memory',  // Z_MEM_ERROR      (-4)
    'Buffer error',         // Z_BUF_ERROR      (-5)
    'Incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);

  {** TZStreamRec ***********************************************************}

  TZStreamRec = packed record
    next_in  : Pointer;   // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : Pointer;   // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : Pointer;   // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;

{** macros ******************************************************************}

function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function deflateInit2(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function inflateInit(var strm: TZStreamRec): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function inflateInit2(var strm: TZStreamRec; windowBits: Integer): Integer;
  {$ifdef Version2005Plus} inline; {$endif}


  const
    {$IFDEF LINUX}
     libz = 'libz.so.1';
    {$ENDIF}
    {$IFDEF DARWIN}
     libz = '/usr/lib/libz.dylib';
    {$ENDIF}
    {$ifdef netware}  {zlib.nlm comes with netware6}
     libz='zlib';
    {$ENDIF}
    {$IFDEF MSWINDOWS}
     //Note that this is the official ZLIB1 .DLL from the http://www.zlib.net/
     libz='zlib1';
     {$ENDIF}


{** external routines *******************************************************}

function Initzlib : Boolean;
procedure Closezlib;
procedure ReadEntryPoints;
function zlibVersion(): string;
function ZLIB_VERSION(): PChar;
function zGetProcAddress(ProcName: PChar): Pointer;

var

zlibVersionpchar: function(): pchar; cdecl;
zErrorpchar: function (err: integer): pchar; cdecl;
inflateSyncPoint: function (z: TZstreamRec): integer; cdecl;
get_crc_table: function (): pointer; cdecl;

deflateInit_: function (var strm: TZStreamRec; level: Integer;
  version: PAnsiChar; recsize: Integer): Integer; cdecl;
deflateInit2_: function (var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer; version: PAnsiChar; recsize: Integer): Integer;
  cdecl;
deflate: function (var strm: TZStreamRec; flush: Integer): Integer;  cdecl;
deflateEnd: function (var strm: TZStreamRec): Integer; cdecl;
deflateReset: function (var strm: TZStreamRec): Integer; cdecl;

inflateInit_: function (var strm: TZStreamRec; version: PAnsiChar;
  recsize: Integer): Integer; cdecl;

inflateInit2_: function (var strm: TZStreamRec; windowBits: Integer;
  version: PAnsiChar; recsize: Integer): Integer ;cdecl;

inflate: function (var strm: TZStreamRec; flush: Integer): Integer; cdecl;

inflateEnd: function (var strm: TZStreamRec): Integer; cdecl;

inflateReset: function (var strm: TZStreamRec): Integer; cdecl;

adler32: function (adler: Longint; const buf; len: Integer): Longint;cdecl;

crc32: function (crc: Longint; const buf; len: Integer): Longint; cdecl;



implementation

Uses
  {$IFDEF GLS_LOGGING}  GLSLog, {$ENDIF}
  {$IFDEF MSWINDOWS}  Windows;  {$ENDIF}
  {$IFDEF GLS_X11_SUPPORT} x, {$ENDIF}
  {$IFDEF UNIX} dynlibs; {$ENDIF}

{*****************************************************************************
*  link zlib code                                                            *
*                                                                            *
*  bcc32 flags                                                               *
*    -c -O2 -Ve -X -pr -a8 -b -d -k- -vi -tWM                                *
*                                                                            *
*  note: do not reorder the following -- doing so will result in external    *
*  functions being undefined                                                 *
*****************************************************************************}


{** macros ******************************************************************}

function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
begin
  result := deflateInit_(strm, level, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function deflateInit2(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
begin
  result := deflateInit2_(strm, level, method, windowBits,
    memLevel, strategy, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function inflateInit(var strm: TZStreamRec): Integer;
begin
  result := inflateInit_(strm, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function inflateInit2(var strm: TZStreamRec; windowBits: Integer): Integer;
begin
  result := inflateInit2_(strm, windowBits, ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

{** external routines *******************************************************}


const
   INVALID_MODULEHANDLE = 0;

var
   {$IFDEF MSWINDOWS}
   vzHandle: HINST;//Pointer;
   {$ENDIF}
   {$IFDEF UNIX}
   vzHandle: TLibHandle = 0;//Pointer;
   {$ENDIF}

function Initzlib: Boolean;
begin
  Result := False;
  if (vzHandle=INVALID_MODULEHANDLE) then
  begin
     Closezlib;
     vzHandle := LoadLibrary(PChar(libz));
     if (vzHandle <> INVALID_MODULEHANDLE) then
      Result := True
     else begin
       if vzHandle <> INVALID_MODULEHANDLE then
         FreeLibrary(vzHandle);
      {$IFDEF GLS_GLS_LOGGING}
        GLSLogger.Log('ZLibEx.pas: Zlib library not loaded');
      {$ENDIF}
     end;
  end
  else Result:=True;
end;

procedure Closezlib;
begin
   if vzHandle<>INVALID_MODULEHANDLE then begin
      FreeLibrary(vzHandle);
      vzHandle:=INVALID_MODULEHANDLE;
   end;
end;

procedure ReadEntryPoints;
begin
  zlibVersionpchar := zGetProcAddress('zlibVersion');
  zErrorpchar := zGetProcAddress('zError');
  inflateSyncPoint := zGetProcAddress('inflateSyncPoint');
  get_crc_table := zGetProcAddress('get_crc_table');
  deflateInit_:= zGetProcAddress('deflateInit_');
  deflateInit2_:= zGetProcAddress('inflateInit2_');
  deflate:= zGetProcAddress('deflate');
  deflateEnd:= zGetProcAddress('deflateEnd');
  deflateReset:= zGetProcAddress('deflateReset');
  inflateInit_:= zGetProcAddress('inflateInit_');
  inflateInit2_:= zGetProcAddress('inflateInit2_');
  inflate:= zGetProcAddress('inflate');
  inflateEnd:= zGetProcAddress('inflateEnd');
  inflateReset:= zGetProcAddress('inflateReset');
  adler32:= zGetProcAddress('adler32');
  crc32:= zGetProcAddress('crc32');
end;

function zlibversion(): string;
begin
   zlibversion := string(zlibversionpchar);
end;

function ZLIB_VERSION(): PChar;
begin
  ZLIB_VERSION := zlibversionpchar;
end;

function zGetProcAddress(ProcName: PChar):Pointer;
begin
  result := GetProcAddress(vzHandle, ProcName);
end;

{** zlib function implementations *******************************************}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

{** c function implementations **********************************************}

procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  FillChar(p^,count,b);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^,dest^,count);
end;

initialization


if Initzlib then ReadEntryPoints;


finalization


Closezlib;


end.
