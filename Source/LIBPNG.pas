//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   History :  
       30/06/11 - DaStr - Fixed compiler crash for Delphi7
       16/02/11 - PREDATOR - Added support for Mac OS X. Tested on Mac OS X 10.6.5.
       01/04/10 - Yar - Bugfix when Delphi not use optimization (thanks Lampogolovii)
       15/03/10 - Yar - Fixed memory leak (thanks Lampogolovii)
       05/03/10 - Yar - Creation
   
}

unit LIBPNG;

interface

{$I GLScene.inc}
{$WARNINGS OFF}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF GLS_X11_SUPPORT} x, {$ENDIF}
  {$IFDEF UNIX} dynlibs, {$ENDIF}
  {$IFDEF GLS_LOGGING} GLSLog, {$ENDIF}
   GLSZLibExAPI,
  Classes, SysUtils, GLVectorGeometry, GLCrossPlatform;

const
{$IFDEF MSWINDOWS}
  LibPng13 = 'libpng13'; // Library name
{$ENDIF}
{$IFDEF LINUX}
  LibPng13 = 'libpng12.so'; // Library name
{$ENDIF}
{$IFDEF DARWIN}
  LibPng13 = '/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources/libPng.dylib'; // Library name
{$ENDIF}

  ZLIB_VERSION = '1.2.3';
  LIBPNG_VERSION = '1.4.0';

  PNG_FLAG_ZLIB_CUSTOM_STRATEGY = $0001;
  PNG_FLAG_ZLIB_CUSTOM_LEVEL = $0002;
  PNG_FLAG_ZLIB_CUSTOM_MEM_LEVEL = $0004;
  PNG_FLAG_ZLIB_CUSTOM_WINDOW_BITS = $0008;
  PNG_FLAG_ZLIB_CUSTOM_METHOD = $0010;
  PNG_FLAG_ZLIB_FINISHED = $0020;
  PNG_FLAG_ROW_INIT = $0040;
  PNG_FLAG_FILLER_AFTER = $0080;
  PNG_FLAG_CRC_ANCILLARY_USE = $0100;
  PNG_FLAG_CRC_ANCILLARY_NOWARN = $0200;
  PNG_FLAG_CRC_CRITICAL_USE = $0400;
  PNG_FLAG_CRC_CRITICAL_IGNORE = $0800;
  PNG_FLAG_KEEP_UNKNOWN_CHUNKS = $8000;
  PNG_FLAG_KEEP_UNSAFE_CHUNKS = $10000;
  PNG_FLAG_LIBRARY_MISMATCH = $20000;
  PNG_FLAG_STRIP_ERROR_NUMBERS = $40000;
  PNG_FLAG_STRIP_ERROR_TEXT = $80000;
  PNG_FLAG_MALLOC_NULL_MEM_OK = $100000;
  PNG_FLAG_ADD_ALPHA = $200000;
  PNG_FLAG_STRIP_ALPHA = $400000;
  PNG_FLAG_BENIGN_ERRORS_WARN = $800000;
  PNG_FLAG_CRC_ANCILLARY_MASK =
    PNG_FLAG_CRC_ANCILLARY_USE or PNG_FLAG_CRC_ANCILLARY_NOWARN;

  PNG_FLAG_CRC_CRITICAL_MASK =
    PNG_FLAG_CRC_CRITICAL_USE or PNG_FLAG_CRC_CRITICAL_IGNORE;

  PNG_FLAG_CRC_MASK = PNG_FLAG_CRC_ANCILLARY_MASK or PNG_FLAG_CRC_CRITICAL_MASK;

  PNG_USER_WIDTH_MAX = 1000000;
  PNG_USER_HEIGHT_MAX = 1000000;
  PNG_UINT_31_MAX = $7FFFFFFF;
  PNG_UINT_32_MAX = $FFFFFFFF;

  PNG_COLOR_MASK_PALETTE = 1;
  PNG_COLOR_MASK_COLOR = 2;
  PNG_COLOR_MASK_ALPHA = 4;

  PNG_COLOR_TYPE_GRAY = 0;
  PNG_COLOR_TYPE_PALETTE = PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_PALETTE;
  PNG_COLOR_TYPE_RGB = PNG_COLOR_MASK_COLOR;
  PNG_COLOR_TYPE_RGB_ALPHA = PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_ALPHA;
  PNG_COLOR_TYPE_GRAY_ALPHA = PNG_COLOR_MASK_ALPHA;

  PNG_INTERLACE_NONE = 0;
  PNG_INTERLACE_ADAM7 = 1;
  PNG_INTERLACE_LAST = 2;

  PNG_COMPRESSION_TYPE_BASE = 0;

  PNG_HAVE_IHDR = $01;
  PNG_HAVE_PLTE = $02;
  PNG_HAVE_IDAT = $04;
  PNG_AFTER_IDAT = $08;
  PNG_HAVE_IEND = $10;
  PNG_HAVE_gAMA = $20;
  PNG_HAVE_cHRM = $40;
  PNG_HAVE_sRGB = $80;
  PNG_HAVE_CHUNK_HEADER = $0100;
  PNG_WROTE_tIME = $0200;
  PNG_WROTE_INFO_BEFORE_PLTE = $0400;
  PNG_BACKGROUND_IS_GRAY = $0800;
  PNG_HAVE_PNG_SIGNATURE = $1000;
  PNG_HAVE_CHUNK_AFTER_IDAT = $2000;

  PNG_INFO_gAMA = $0001;
  PNG_INFO_sBIT = $0002;
  PNG_INFO_cHRM = $0004;
  PNG_INFO_PLTE = $0008;
  PNG_INFO_tRNS = $0010;
  PNG_INFO_bKGD = $0020;
  PNG_INFO_hIST = $0040;
  PNG_INFO_pHYs = $0080;
  PNG_INFO_oFFs = $0100;
  PNG_INFO_tIME = $0200;
  PNG_INFO_pCAL = $0400;
  PNG_INFO_sRGB = $0800;
  PNG_INFO_iCCP = $1000;
  PNG_INFO_sPLT = $2000;
  PNG_INFO_sCAL = $4000;
  PNG_INFO_IDAT = $8000;

  PNG_FILTER_TYPE_BASE = 0;
  PNG_INTRAPIXEL_DIFFERENCING = 64;

  PNG_FLAG_MNG_EMPTY_PLTE = $01;
  PNG_FLAG_MNG_FILTER_64 = $04;
  PNG_ALL_MNG_FEATURES = $05;

  PNG_BGR = $0001;
  PNG_INTERLACE = $0002;
  PNG_PACK = $0004;
  PNG_SHIFT = $0008;
  PNG_SWAP_BYTES = $0010;
  PNG_INVERT_MONO = $0020;
  PNG_DITHER = $0040;
  PNG_BACKGROUND = $0080;
  PNG_BACKGROUND_EXPAND = $0100;

  PNG_16_TO_8 = $0400;
  PNG_RGBA = $0800;
  PNG_EXPAND = $1000;
  PNG_GAMMA = $2000;
  PNG_GRAY_TO_RGB = $4000;
  PNG_FILLER = $8000;
  PNG_PACKSWAP = $10000;
  PNG_SWAP_ALPHA = $20000;
  PNG_STRIP_ALPHA = $40000;
  PNG_INVERT_ALPHA = $80000;
  PNG_USER_TRANSFORM = $100000;
  PNG_RGB_TO_GRAY_ERR = $200000;
  PNG_RGB_TO_GRAY_WARN = $400000;
  PNG_RGB_TO_GRAY = $600000;

  PNG_ADD_ALPHA = $1000000;
  PNG_EXPAND_tRNS = $2000000;

  PNG_GAMMA_THRESHOLD = 0.05;

  PNG_BACKGROUND_GAMMA_UNKNOWN = 0;
  PNG_BACKGROUND_GAMMA_SCREEN = 1;
  PNG_BACKGROUND_GAMMA_FILE = 2;
  PNG_BACKGROUND_GAMMA_UNIQUE = 3;

  PNG_FREE_HIST = $0008;
  PNG_FREE_ICCP = $0010;
  PNG_FREE_SPLT = $0020;
  PNG_FREE_ROWS = $0040;
  PNG_FREE_PCAL = $0080;
  PNG_FREE_SCAL = $0100;
  PNG_FREE_UNKN = $0200;
  PNG_FREE_LIST = $0400;
  PNG_FREE_PLTE = $1000;
  PNG_FREE_TRNS = $2000;
  PNG_FREE_TEXT = $4000;
  PNG_FREE_ALL = $7FFF;
  PNG_FREE_MUL = $4220;

  PNG_COMPRESSION_TYPE_DEFAULT = PNG_COMPRESSION_TYPE_BASE;
  PNG_FILTER_TYPE_DEFAULT = PNG_FILTER_TYPE_BASE;

type
  time_t = longint;
  int = longint;

  png_uint_32 = dword;
  png_int_32 = longint;
  png_uint_16 = word;
  png_int_16 = smallint;
  png_byte = byte;
  ppng_uint_32 = ^png_uint_32;
  ppng_int_32 = ^png_int_32;
  ppng_uint_16 = ^png_uint_16;
  ppng_int_16 = ^png_int_16;
  ppng_byte = ^png_byte;
  pppng_uint_32 = ^ppng_uint_32;
  pppng_int_32 = ^ppng_int_32;
  pppng_uint_16 = ^ppng_uint_16;
  ppng_uint_16p = ^png_uint_16p;
  pppng_int_16 = ^ppng_int_16;
  pppng_byte = ^ppng_byte;
  png_size_t = ptruint;
  png_fixed_point = png_int_32;
  ppng_fixed_point = ^png_fixed_point;
  pppng_fixed_point = ^ppng_fixed_point;
  png_voidp = pointer;
  ppng_bytep = ^png_bytep;
  (* Const before type ignored *)
  png_const_charp = PChar;
  png_charp = PChar;
  ppng_charp = ^png_charp;
  png_fixed_point_p = Ppng_fixed_point;
  TFile = Pointer;
  png_FILE_p = ^file;
  png_doublep = Pdouble;

  png_byte_array = array[0..MaxInt div 2 - 1] of png_byte;
  png_bytep = ^png_byte_array;
  png_bytep_array = array[0..MaxInt div (2 * SizeOf(png_bytep))] of png_bytep;
  png_bytep_arrayp = ^png_bytep_array;
  png_bytepp = ^png_bytep;
  png_uint_32p = ^png_uint_32;
  png_int_32p = PInteger;
  png_uint_16p = ^png_uint_16;
  png_uint_16pp = ^png_uint_16p;
  png_int_16p = PShortInt;
  png_charp_array = array[0..MaxInt div (2 * SizeOf(png_charp))] of png_charp;
  png_charp_arrayp = ^png_charp_array;
  png_charpp = ^png_charp;
  ppng_charpp = ^png_charpp;

  jmp_buf = record
    j_ebp,
    j_ebx,
    j_edi,
    j_esi,
    j_esp,
    j_ret,
    j_excep,
    j_context: cardinal;
  end;
  jmp_bufp = ^jmp_buf;

  { Three color definitions.  The order of the red, green, and blue, (and the
    exact size) is not important, although the size of the fields need to
    be png_byte or png_uint_16 (as defined below). }
  png_color = record
    red: png_byte;
    green: png_byte;
    blue: png_byte;
  end;
  png_color_array = array[0..MaxInt div (2 * SizeOf(png_color))] of png_color;
  png_colorp = ^png_color_array;
  png_colorpp = ^png_colorp;

  png_color_8 = record
    red: png_byte; // for use in red green blue files
    green: png_byte;
    blue: png_byte;
    gray: png_byte; // for use in grayscale files
    alpha: png_byte; // for alpha channel files
  end;
  png_color_8p = ^png_color_8;
  png_color_8pp = ^png_color_8p;
  ppng_color_8p = ^png_color_8p;

  ppng_colorp = ^png_colorp;
  png_color_16 = record
    index: png_byte; // used for palette files
    red: png_uint_16; // for use in red green blue files
    green: png_uint_16;
    blue: png_uint_16;
    gray: png_uint_16; // for use in grayscale files
  end;
  png_color_16p = ^png_color_16;
  png_color_16pp = ^png_color_16p;
  ppng_color_16p = ^png_color_16p;

  { png_unknown_chunk is a structure to hold queued chunks for which there is
     no specific support.  The idea is that we can use this to queue
     up private chunks for output even though the library doesn't actually
     know about their semantics. }

  png_unknown_chunk = record
    Name: array[0..4] of png_byte;
    Data: png_bytep;
    size: png_size_t;

    { libpng-using applications should NOT directly modify this byte.
       png_byte location;  mode of operation at read time }
  end;
  png_unknown_chunk_array =
    array[0..MaxInt div (2 * SizeOf(png_unknown_chunk))] of png_unknown_chunk;
  png_unknown_chunk_arrayp = ^png_unknown_chunk_array;
  png_unknown_chunkp = ^png_unknown_chunk;
  png_unknown_chunkpp = ^png_unknown_chunkp;

  png_text = record
    { compression value:
       -1: tEXt, none
        0: zTXt, deflate
        1: iTXt, none
        2: iTXt, deflate  }
    compression: integer;
    { keyword, 1-79 character description of "text" }
    key: png_charp;
    { comment, may be an empty string (ie "")
       or a NULL pointer }
    Text: png_charp;
    { length of the text string }
    text_length: png_size_t;
    { length of the itxt string }
    itxt_length: png_size_t;
    { language code, 0-79 characters
       or a NULL pointer }
    lang: png_charp;
    { keyword translated UTF-8 string, 0 or more
       chars or a NULL pointer }
    lang_key: png_charp;
  end;
  png_text_array = array[0..MaxInt div (2 * SizeOf(png_text))] of png_text;
  png_text_arrayp = ^png_text_array;
  png_textp = ^png_text;
  png_textpp = ^png_textp;
  ppng_textp = ^png_textp;

  png_time = record
    year: png_uint_16; { full year, as in, 1995 }
    month: png_byte; { month of year, 1 - 12 }
    day: png_byte; { day of month, 1 - 31 }
    hour: png_byte; { hour of day, 0 - 23 }
    minute: png_byte; { minute of hour, 0 - 59 }
    second: png_byte; { second of minute, 0 - 60 (for leap seconds) }
  end;
  png_timep = ^png_time;
  png_timepp = ^png_timep;
  ppng_timep = ^png_timep;

  png_sPLT_entry = record
    red: png_uint_16;
    green: png_uint_16;
    blue: png_uint_16;
    alpha: png_uint_16;
    frequency: png_uint_16;
  end;

  png_sPLT_entryp = ^png_sPLT_entry;
  png_sPLT_entrypp = ^png_sPLT_entryp;

  png_sPLT_t = record
    Name: png_charp; { palette name }
    depth: png_byte; { depth of palette samples }
    entries: png_sPLT_entryp; { palette entries }
    nentries: png_int_32; { number of palette entries }
  end;
  png_sPLT_tp_array =
    array[0..MaxInt div (2 * SizeOf(png_sPLT_t))] of png_sPLT_t;
  png_sPLT_tp_arrayp = ^png_sPLT_tp_array;
  png_sPLT_tp = ^png_sPLT_t;
  png_sPLT_tpp = ^png_sPLT_tp;

  png_structp = ^png_struct;
  png_structpp = ^png_structp;

  png_info = record
    Width: png_uint_32; { width of image in pixels (from IHDR) }
    Height: png_uint_32; { height of image in pixels (from IHDR) }
    valid: png_uint_32; { valid chunk data (see PNG_INFO_ below) }
    rowbytes: png_size_t; { bytes needed to hold an untransformed row }
    palette: png_colorp; { array of color values (valid & PNG_INFO_PLTE) }
    num_palette: png_uint_16; { number of color entries in "palette" (PLTE) }
    num_trans: png_uint_16; { number of transparent palette color (tRNS) }
    bit_depth: png_byte; { 1, 2, 4, 8, or 16 bits/channel (from IHDR) }
    color_type: png_byte; { see PNG_COLOR_TYPE_ below (from IHDR) }

    compression_type: png_byte; { must be PNG_COMPRESSION_TYPE_BASE (IHDR) }
    filter_type: png_byte; { must be PNG_FILTER_TYPE_BASE (from IHDR) }
    interlace_type: png_byte; { One of PNG_INTERLACE_NONE, PNG_INTERLACE_ADAM7 }

    channels: png_byte; { number of data channels per pixel (1, 2, 3, 4) }
    pixel_depth: png_byte; { number of bits per pixel }
    spare_byte: png_byte; { to align the data, and for future use }
    signature: array[0..7] of png_byte;
    { magic bytes read by libpng from start of file }

    gamma: single; { gamma value of image, if (valid & PNG_INFO_gAMA) }
    srgb_intent: png_byte; { sRGB rendering intent [0, 1, 2, or 3] }

    num_text: integer; { number of comments read/to write }
    max_text: integer; { current size of text array }
    Text: png_textp; { array of comments read/to write }

    mod_time: png_time;

    sig_bit: png_color_8; { significant bits in color channels }

    trans_alpha: png_bytep; { alpha values for paletted image }
    trans_color: png_color_16; { transparent color for non-palette image }

    background: png_color_16;

    x_offset: png_int_32; { x offset on page }
    y_offset: png_int_32; { y offset on page }
    offset_unit_type: png_byte; { offset units type }

    x_pixels_per_unit: png_uint_32; { horizontal pixel density }
    y_pixels_per_unit: png_uint_32; { vertical pixel density }
    phys_unit_type: png_byte; { resolution type (see PNG_RESOLUTION_ below) }

    hist: png_uint_16p;

    x_white: single;
    y_white: single;
    x_red: single;
    y_red: single;
    x_green: single;
    y_green: single;
    x_blue: single;
    y_blue: single;

    pcal_purpose: png_charp; { pCAL chunk description string }
    pcal_X0: png_int_32; { minimum value }
    pcal_X1: png_int_32; { maximum value }
    pcal_units: png_charp; { Latin-1 string giving physical units }
    pcal_params: png_charpp; { ASCII strings containing parameter values }
    pcal_type: png_byte; { equation type (see PNG_EQUATION_ below) }
    pcal_nparams: png_byte; { number of parameters given in pcal_params }

    free_me: png_uint_32; { flags items libpng is responsible for freeing }

    unknown_chunks: png_unknown_chunkp;
    unknown_chunks_num: png_size_t;

    iccp_name: png_charp; { profile name }
    iccp_profile: png_charp; { International Color Consortium profile data }
    { Note to maintainer: should be png_bytep }
    iccp_proflen: png_uint_32; { ICC profile data length }
    iccp_compression: png_byte; { Always zero }

    splt_palettes: png_sPLT_tp;
    splt_palettes_num: png_uint_32;

    scal_unit: png_byte; { unit of physical scale }
    scal_pixel_width: double; { width of one pixel }
    scal_pixel_height: double; { height of one pixel }
    scal_s_width: png_charp; { string containing height }
    scal_s_height: png_charp; { string containing width }

    row_pointers: png_bytepp; { the image bits }

    int_gamma: png_fixed_point; { gamma of image, if (valid & PNG_INFO_gAMA) }

    int_x_white: png_fixed_point;
    int_y_white: png_fixed_point;
    int_x_red: png_fixed_point;
    int_y_red: png_fixed_point;
    int_x_green: png_fixed_point;
    int_y_green: png_fixed_point;
    int_x_blue: png_fixed_point;
    int_y_blue: png_fixed_point;
  end;
  png_infop = ^png_info;
  png_infopp = ^png_infop;

  png_row_info = record
    Width: png_uint_32; // width of row
    rowbytes: png_uint_32; // number of bytes in row
    color_type: png_byte; // color type of row
    bit_depth: png_byte; // bit depth of row
    channels: png_byte; // number of channels (1, 2, 3, or 4)
    pixel_depth: png_byte; // bits per pixel (depth * channels)
  end;
  png_row_infop = ^png_row_info;

  TAlloc = function(AppData: Pointer; Items, Size: integer): Pointer; cdecl;
  TFree = procedure(AppData, Block: Pointer); cdecl;
  TInFunc = function(opaque: Pointer; var buf: PByte): integer; cdecl;
  TOutFunc = function(opaque: Pointer; buf: PByte; size: integer): integer;
  png_error_ptr = procedure(struct: png_structp; str: png_const_charp); cdecl;
  png_rw_ptr = procedure(struct: png_structp; Data: png_bytep;
    size: png_size_t); cdecl;
  png_longjmp_ptr = procedure(jb: jmp_buf; i: integer); cdecl;
  png_user_transform_ptr = procedure(struct: png_structp;
    row_info: png_row_infop; b: png_bytep); cdecl;
  png_flush_ptr = procedure(struct: png_structp); cdecl;
  png_read_status_ptr = procedure(struct: png_structp; ui: png_uint_32;
    i: integer); cdecl;
  png_write_status_ptr = procedure(struct: png_structp; ui: png_uint_32;
    i: integer); cdecl;
  png_progressive_info_ptr = procedure(struct: png_structp; info: png_infop);
    cdecl;
  png_progressive_end_ptr = procedure(struct: png_structp; info: png_infop);
    cdecl;
  png_progressive_row_ptr = procedure(struct: png_structp; bp: png_bytep;
    ui: png_uint_32; i: integer); cdecl;
  png_malloc_ptr = function(struct: png_structp; size: png_size_t): png_voidp;
  png_free_ptr = procedure(struct: png_structp; ptr: png_voidp);
  png_user_chunk_ptr = function(struct: png_structp;
    chunk: png_unknown_chunkp): integer;

  // Internal structure.  Ignore.
  TZStreamRec = packed record
    next_in: PAnsiChar; // next input byte
    avail_in: integer; // number of bytes available at next_in
    total_in: integer; // total nb of input bytes read so far

    next_out: PAnsiChar; // next output byte should be put here
    avail_out: integer; // remaining free space at next_out
    total_out: integer; // total nb of bytes output so far

    msg: PAnsiChar; // last error message, NULL if no error
    internal: Pointer; // not visible by applications

    zalloc: TAlloc; // used to allocate the internal state
    zfree: TFree; // used to free the internal state
    AppData: Pointer; // private data object passed to zalloc and zfree

    data_type: integer; //  best guess about the data type: ascii or binary
    adler: integer; // adler32 value of the uncompressed data
    reserved: integer; // reserved for future use
  end;

  png_struct = record
    jmpbuf: jmp_buf; // used in png_error
    longjmp_fn: png_longjmp_ptr; // setjmp non-local goto function.
    error_fn: png_error_ptr; // function for printing errors and aborting
    warning_fn: png_error_ptr; // function for printing warnings
    error_ptr: png_voidp; // user supplied struct for error functions
    write_data_fn: png_rw_ptr; // function for writing output data
    read_data_fn: png_rw_ptr; // function for reading input data
    io_ptr: png_voidp; // ptr to application struct for I/O functions

    read_user_transform_fn: png_user_transform_ptr; // user read transform
    write_user_transform_fn: png_user_transform_ptr; // user write transform

    // These were added in libpng-1.0.2
    user_transform_ptr: png_voidp; // user supplied struct for user transform
    user_transform_depth: png_byte; // bit depth of user transformed pixels
    user_transform_channels: png_byte; // channels in user transformed pixels

    mode: png_uint_32; // tells us where we are in the PNG file
    flags: png_uint_32; // flags indicating various things to libpng
    transformations: png_uint_32; // which transformations to perform

    zstream: TZStreamRec; // pointer to decompression structure (below)
    zbuf: png_bytep; // buffer for zlib
    zbuf_size: png_size_t; // size of zbuf
    zlib_level: integer; // holds zlib compression level
    zlib_method: integer; // holds zlib compression method
    zlib_window_bits: integer; // holds zlib compression window bits
    zlib_mem_level: integer; // holds zlib compression memory level
    zlib_strategy: integer; // holds zlib compression strategy

    Width: png_uint_32; // width of image in pixels
    Height: png_uint_32; // height of image in pixels
    num_rows: png_uint_32; // number of rows in current pass
    usr_width: png_uint_32; // width of row at start of write
    rowbytes: png_uint_32; // size of row in bytes
    irowbytes: png_uint_32; // size of current interlaced row in bytes
    iwidth: png_uint_32; // width of current interlaced row in pixels
    row_number: png_uint_32; // current row in interlace pass
    prev_row: png_bytep; // buffer to save previous (unfiltered) row
    row_buf: png_bytep; // buffer to save current (unfiltered) row
    sub_row: png_bytep; // buffer to save "sub" row when filtering
    up_row: png_bytep; // buffer to save "up" row when filtering
    avg_row: png_bytep; // buffer to save "avg" row when filtering
    paeth_row: png_bytep; // buffer to save "Paeth" row when filtering
    row_info: png_row_info; // used for transformation routines

    idat_size: png_uint_32; // current IDAT size for read
    crc: png_uint_32; // current chunk CRC value
    palette: png_colorp; // palette from the input file
    num_palette: png_uint_16; // number of color entries in palette
    num_trans: png_uint_16; // number of transparency values
    chunk_name: array[0..4] of png_byte; // null-terminated name of current chunk
    compression: png_byte; // file compression type (always 0)
    filter: png_byte; // file filter type (always 0)
    interlaced: png_byte; // PNG_INTERLACE_NONE, PNG_INTERLACE_ADAM7
    pass: png_byte; // current interlace pass (0 - 6)
    do_filter: png_byte; // row filter flags (see PNG_FILTER_ below )
    color_type: png_byte; // color type of file
    bit_depth: png_byte; // bit depth of file
    usr_bit_depth: png_byte; // bit depth of users row
    pixel_depth: png_byte; // number of bits per pixel
    channels: png_byte; // number of channels in file
    usr_channels: png_byte; // channels at start of write
    sig_bytes: png_byte; // magic bytes read/written from start of file

    filler: png_uint_16; // filler bytes for pixel expansion

    background_gamma_type: png_byte;
    background_gamma: single;
    background: png_color_16; // background color in screen gamma space
    background_1: png_color_16; // background normalized to gamma 1.0

    output_flush_fn: png_flush_ptr; // Function for flushing output
    flush_dist: png_uint_32; // how many rows apart to flush, 0 - no flush
    flush_rows: png_uint_32; // number of rows written since last flush

    gamma_shift: integer; // number of "insignificant" bits 16-bit gamma
    gamma: single; // file gamma value
    screen_gamma: single; // screen gamma value (display_exponent)

    gamma_table: png_bytep; // gamma table for 8-bit depth files
    gamma_from_1: png_bytep; // converts from 1.0 to screen
    gamma_to_1: png_bytep; // converts from file to 1.0
    gamma_16_table: png_uint_16pp; // gamma table for 16-bit depth files
    gamma_16_from_1: png_uint_16pp; // converts from 1.0 to screen
    gamma_16_to_1: png_uint_16pp; // converts from file to 1.0

    sig_bit: png_color_8; // significant bits in each available channel

    shift: png_color_8; // shift for significant bit tranformation

    trans: png_bytep; // transparency values for paletted files
    trans_values: png_color_16; // transparency values for non-paletted files

    trans_alpha: png_bytep; // alpha values for paletted files
    trans_color: png_color_16; // transparent color for non-paletted files

    read_row_fn: png_read_status_ptr; // called after each row is decoded
    write_row_fn: png_write_status_ptr; // called after each row is encoded

    info_fn: png_progressive_info_ptr; // called after header data fully read
    row_fn: png_progressive_row_ptr; // called after each prog. row is decoded
    end_fn: png_progressive_end_ptr; // called after image is complete
    save_buffer_ptr: png_bytep; // current location in save_buffer
    save_buffer: png_bytep; // buffer for previously read data
    current_buffer_ptr: png_bytep; // current location in current_buffer
    current_buffer: png_bytep; // buffer for recently used data
    push_length: png_uint_32; // size of current input chunk
    skip_length: png_uint_32; // bytes to skip in input data
    save_buffer_size: png_size_t; // amount of data now in save_buffer
    save_buffer_max: png_size_t; // total size of save_buffer
    buffer_size: png_size_t; // total amount of available input data
    current_buffer_size: png_size_t; // amount of data now in current_buffer
    process_mode: integer; // what push library is currently doing
    cur_palette: integer; // current push library palette index

    current_text_size: png_size_t; // current size of text input data
    current_text_left: png_size_t; // how much text left to read in input
    current_text: png_charp; // current text chunk buffer
    current_text_ptr: png_charp; // current location in current_text

    hist: png_uint_16p; // histogram }

    heuristic_method: png_byte; // heuristic for row filter selection }
    num_prev_filters: png_byte; // number of weights for previous rows }
    prev_filters: png_bytep; // filter type(s) of previous row(s) }
    filter_weights: png_uint_16p; // weight(s) for previous line(s) }
    inv_filter_weights: png_uint_16p; // 1/weight(s) for previous line(s) }
    filter_costs: png_uint_16p; // relative filter calculation cost }
    inv_filter_costs: png_uint_16p; // 1/relative filter calculation cost }

    time_buffer: png_charp; // String to hold RFC 1123 time text

    // New members added in libpng-1.0.6

    free_me: png_uint_32; // flags items libpng is responsible for freeing

    user_chunk_ptr: png_voidp;
    read_user_chunk_fn: png_user_chunk_ptr; // user read chunk handler

    num_chunk_list: integer;
    chunk_list: png_bytep;

    // New members added in libpng-1.0.3
    rgb_to_gray_status: png_byte;
    // These were changed from png_byte in libpng-1.0.6
    rgb_to_gray_red_coeff: png_uint_16;
    rgb_to_gray_green_coeff: png_uint_16;
    rgb_to_gray_blue_coeff: png_uint_16;

    // New member added in libpng-1.0.4 (renamed in 1.0.9)
    // Changed from png_byte to png_uint_32 at version 1.2.0
    mng_features_permitted: png_uint_32;

    // New member added in libpng-1.0.7
    int_gamma: png_fixed_point;

    // New member added in libpng-1.0.9, ifdef'ed out in 1.0.12, enabled in 1.2.0
    filter_type: png_byte;

    // New members added in libpng-1.2.0

    // New members added in libpng-1.0.2 but first enabled by default in 1.2.0
    mem_ptr: png_voidp; // user supplied struct for mem functions
    malloc_fn: png_malloc_ptr; // function for allocating memory
    free_fn: png_free_ptr; // function for freeing memory

    // New member added in libpng-1.0.13 and 1.2.0
    big_row_buf: png_bytep; // buffer to save current (unfiltered) row

    // New members added in libpng-1.0.16 and 1.2.6
    compression_type: png_byte;

    //    user_width_max: png_uint_32;
    //    user_height_max: png_uint_32;
    // Added in libpng-1.4.0: Total number of sPLT, text, and unknown
    // chunks that can be stored ( $7fffffff means unlimited).

    //    user_chunk_cache_max: png_uint_32;

    // New member added in libpng-1.0.25 and 1.2.17
    // Storage for unknown chunk that the library doesn't recognize.
    unknown_chunk: png_unknown_chunk;

    // New members added in libpng-1.2.26
    old_big_row_buf_size: png_uint_32;
    old_prev_row_size: png_uint_32;

    // New member added in libpng-1.2.30
    chunkdata: png_charp; // buffer for reading chunk data

    // New member added in libpng-1.4.0
    io_state: png_uint_32;
  end;

(* stream read/write function *)
procedure pngReadFn(png_ptr: png_structp; Data: png_bytep; length: png_size_t);
  cdecl;
procedure pngWriteFn(png_ptr: png_structp; Data: png_bytep; length: png_size_t);
  cdecl;
procedure pngErrorFn(struct: png_structp; str: png_const_charp);
  cdecl;
procedure pngWarnFn(struct: png_structp; str: png_const_charp);
  cdecl;

var
  _png_access_version_number: function(): png_uint_32; cdecl;
  _png_set_sig_bytes: procedure(png_ptr: png_structp; num_bytes: longint); cdecl;
  _png_sig_cmp: function(sig: png_bytep; start: png_size_t;
  num_to_check: png_size_t): longint; cdecl;
  _png_check_sig: function(sig: png_bytep; num: longint): longint; cdecl;
  _png_create_read_struct: function(user_png_ver: png_const_charp;
  error_ptr: png_voidp; error_fn: png_error_ptr;
  warn_fn: png_error_ptr): png_structp; cdecl;
  _png_create_write_struct: function(user_png_ver: png_const_charp;
  error_ptr: png_voidp; error_fn: png_error_ptr;
  warn_fn: png_error_ptr): png_structp; cdecl;
  _png_get_compression_buffer_size: function(png_ptr: png_structp): png_uint_32; cdecl;
  _png_set_compression_buffer_size: procedure(png_ptr: png_structp;
  size: png_uint_32); cdecl;
  _png_reset_zstream: function(png_ptr: png_structp): longint; cdecl;
  _png_write_chunk: procedure(png_ptr: png_structp; chunk_name: png_bytep;
  Data: png_bytep; length: png_size_t); cdecl;
  _png_write_chunk_start: procedure(png_ptr: png_structp; chunk_name: png_bytep;
  length: png_uint_32); cdecl;
  _png_write_chunk_data: procedure(png_ptr: png_structp; Data: png_bytep;
  length: png_size_t); cdecl;
  _png_write_chunk_end: procedure(png_ptr: png_structp); cdecl;
  _png_create_info_struct: function(png_ptr: png_structp): png_infop; cdecl;
  _png_info_init: procedure(info_ptr: png_infop); cdecl;
  _png_write_info_before_PLTE: procedure(png_ptr: png_structp;
  info_ptr: png_infop); cdecl;
  _png_write_info: procedure(png_ptr: png_structp; info_ptr: png_infop); cdecl;
  _png_read_info: procedure(png_ptr: png_structp; info_ptr: png_infop); cdecl;
  _png_convert_to_rfc1123: function(png_ptr: png_structp;
  ptime: png_timep): png_charp; cdecl;
  _png_convert_from_struct_tm: procedure(ptime: png_timep; ttime: Pointer); cdecl;
  _png_convert_from_time_t: procedure(ptime: png_timep; ttime: time_t); cdecl;
  _png_set_expand: procedure(png_ptr: png_structp); cdecl;
  _png_set_gray_1_2_4_to_8: procedure(png_ptr: png_structp); cdecl;
  _png_set_palette_to_rgb: procedure(png_ptr: png_structp); cdecl;
  _png_set_tRNS_to_alpha: procedure(png_ptr: png_structp); cdecl;
  _png_set_bgr: procedure(png_ptr: png_structp); cdecl;
  _png_set_gray_to_rgb: procedure(png_ptr: png_structp); cdecl;
  _png_set_rgb_to_gray: procedure(png_ptr: png_structp; error_action: longint;
  red: double; green: double); cdecl;
  _png_set_rgb_to_gray_fixed: procedure(png_ptr: png_structp;
  error_action: longint; red: png_fixed_point; green: png_fixed_point); cdecl;
  _png_get_rgb_to_gray_status: function(png_ptr: png_structp): png_byte; cdecl;
  _png_build_grayscale_palette: procedure(bit_depth: longint; palette: png_colorp); cdecl;
  _png_set_strip_alpha: procedure(png_ptr: png_structp); cdecl;
  _png_set_swap_alpha: procedure(png_ptr: png_structp); cdecl;
  _png_set_invert_alpha: procedure(png_ptr: png_structp); cdecl;
  _png_set_filler: procedure(png_ptr: png_structp; filler: png_uint_32;
  flags: longint); cdecl;
  _png_set_swap: procedure(png_ptr: png_structp); cdecl;
  _png_set_packing: procedure(png_ptr: png_structp); cdecl;
  _png_set_packswap: procedure(png_ptr: png_structp); cdecl;
  _png_set_shift: procedure(png_ptr: png_structp; true_bits: png_color_8p); cdecl;
  _png_set_interlace_handling: function(png_ptr: png_structp): longint; cdecl;
  _png_set_invert_mono: procedure(png_ptr: png_structp); cdecl;
  _png_set_background: procedure(png_ptr: png_structp; background_color: png_color_16p;
  background_gamma_code: longint; need_expand: longint; background_gamma: double); cdecl;
  _png_set_strip_16: procedure(png_ptr: png_structp); cdecl;
  _png_set_dither: procedure(png_ptr: png_structp; palette: png_colorp;
  num_palette: longint; maximum_colors: longint; histogram: png_uint_16p;
  full_dither: longint); cdecl;
  _png_set_gamma: procedure(png_ptr: png_structp; screen_gamma: double;
  default_file_gamma: double); cdecl;
  _png_permit_empty_plte: procedure(png_ptr: png_structp;
  empty_plte_permitted: longint); cdecl;
  _png_set_flush: procedure(png_ptr: png_structp; nrows: longint); cdecl;
  _png_write_flush: procedure(png_ptr: png_structp); cdecl;
  _png_start_read_image: procedure(png_ptr: png_structp); cdecl;
  _png_read_update_info: procedure(png_ptr: png_structp; info_ptr: png_infop); cdecl;
  _png_read_rows: procedure(png_ptr: png_structp; row: png_bytepp;
  display_row: png_bytepp; num_rows: png_uint_32); cdecl;
  _png_read_row: procedure(png_ptr: png_structp; row: png_bytep;
  display_row: png_bytep); cdecl;
  _png_read_image: procedure(png_ptr: png_structp; image: png_bytepp); cdecl;
  _png_write_row: procedure(png_ptr: png_structp; row: png_bytep); cdecl;
  _png_write_rows: procedure(png_ptr: png_structp; row: png_bytepp;
  num_rows: png_uint_32); cdecl;
  _png_write_image: procedure(png_ptr: png_structp; image: png_bytepp); cdecl;
  _png_write_end: procedure(png_ptr: png_structp; info_ptr: png_infop); cdecl;
  _png_read_end: procedure(png_ptr: png_structp; info_ptr: png_infop); cdecl;
  _png_destroy_info_struct: procedure(png_ptr: png_structp;
  info_ptr_ptr: png_infopp); cdecl;
  _png_destroy_read_struct: procedure(png_ptr_ptr: png_structpp;
  info_ptr_ptr: png_infopp; end_info_ptr_ptr: png_infopp); cdecl;
  _png_read_destroy: procedure(png_ptr: png_structp; info_ptr: png_infop;
  end_info_ptr: png_infop); cdecl;
  _png_destroy_write_struct: procedure(png_ptr_ptr: png_structpp;
  info_ptr_ptr: png_infopp); cdecl;
  _png_write_destroy_info: procedure(info_ptr: png_infop); cdecl;
  _png_write_destroy: procedure(png_ptr: png_structp); cdecl;
  _png_set_crc_action: procedure(png_ptr: png_structp; crit_action: longint;
  ancil_action: longint); cdecl;
  _png_set_filter: procedure(png_ptr: png_structp; method: longint;
  filters: longint); cdecl;
  _png_set_filter_heuristics: procedure(png_ptr: png_structp;
  heuristic_method: longint; num_weights: longint; filter_weights: png_doublep;
  filter_costs: png_doublep); cdecl;
  _png_set_compression_level: procedure(png_ptr: png_structp; level: longint); cdecl;
  _png_set_compression_mem_level: procedure(png_ptr: png_structp;
  mem_level: longint); cdecl;
  _png_set_compression_strategy: procedure(png_ptr: png_structp;
  strategy: longint); cdecl;
  _png_set_compression_window_bits: procedure(png_ptr: png_structp;
  window_bits: longint); cdecl;
  _png_set_compression_method: procedure(png_ptr: png_structp; method: longint); cdecl;
  _png_init_io: procedure(png_ptr: png_structp; fp: png_FILE_p); cdecl;
  _png_set_error_fn: procedure(png_ptr: png_structp; error_ptr: png_voidp;
  error_fn: png_error_ptr; warning_fn: png_error_ptr); cdecl;
  _png_get_error_ptr: function(png_ptr: png_structp): png_voidp; cdecl;
  _png_set_write_fn: procedure(png_ptr: png_structp; io_ptr: png_voidp;
  write_data_fn: png_rw_ptr; output_flush_fn: png_flush_ptr); cdecl;
  _png_set_read_fn: procedure(png_ptr: png_structp; io_ptr: png_voidp;
  read_data_fn: png_rw_ptr); cdecl;
  _png_get_io_ptr: function(png_ptr: png_structp): png_voidp; cdecl;
  _png_set_read_status_fn: procedure(png_ptr: png_structp;
  read_row_fn: png_read_status_ptr); cdecl;
  _png_set_write_status_fn: procedure(png_ptr: png_structp;
  write_row_fn: png_write_status_ptr); cdecl;
  _png_set_read_user_transform_fn: procedure(png_ptr: png_structp;
  read_user_transform_fn: png_user_transform_ptr); cdecl;
  _png_set_write_user_transform_fn: procedure(png_ptr: png_structp;
  write_user_transform_fn: png_user_transform_ptr); cdecl;
  _png_set_user_transform_info: procedure(png_ptr: png_structp;
  user_transform_ptr: png_voidp; user_transform_depth: longint;
  user_transform_channels: longint); cdecl;
  _png_get_user_transform_ptr: function(png_ptr: png_structp): png_voidp; cdecl;
  _png_set_read_user_chunk_fn: procedure(png_ptr: png_structp;
  user_chunk_ptr: png_voidp; read_user_chunk_fn: png_user_chunk_ptr); cdecl;
  _png_get_user_chunk_ptr: function(png_ptr: png_structp): png_voidp; cdecl;
  _png_set_progressive_read_fn: procedure(png_ptr: png_structp;
  progressive_ptr: png_voidp; info_fn: png_progressive_info_ptr;
  row_fn: png_progressive_row_ptr; end_fn: png_progressive_end_ptr); cdecl;
  _png_get_progressive_ptr: function(png_ptr: png_structp): png_voidp; cdecl;
  _png_process_data: procedure(png_ptr: png_structp; info_ptr: png_infop;
  buffer: png_bytep; buffer_size: png_size_t); cdecl;
  _png_progressive_combine_row: procedure(png_ptr: png_structp;
  old_row: png_bytep; new_row: png_bytep); cdecl;
  _png_malloc: function(png_ptr: png_structp; size: png_uint_32): png_voidp; cdecl;
  _png_free: procedure(png_ptr: png_structp; ptr: png_voidp); cdecl;
  _png_free_data: procedure(png_ptr: png_structp; info_ptr: png_infop;
  free_me: png_uint_32; num: longint); cdecl;
  _png_data_freer: procedure(png_ptr: png_structp; info_ptr: png_infop;
  freer: longint; mask: png_uint_32); cdecl;
  _png_memcpy_check: function(png_ptr: png_structp; s1: png_voidp;
  s2: png_voidp; size: png_uint_32): png_voidp; cdecl;
  _png_memset_check: function(png_ptr: png_structp; s1: png_voidp;
  Value: longint; size: png_uint_32): png_voidp; cdecl;
  _png_error: procedure(png_ptr: png_structp; error: png_const_charp); cdecl;
  _png_chunk_error: procedure(png_ptr: png_structp; error: png_const_charp); cdecl;
  _png_warning: procedure(png_ptr: png_structp; message: png_const_charp); cdecl;
  _png_chunk_warning: procedure(png_ptr: png_structp; message: png_const_charp); cdecl;
  _png_get_valid: function(png_ptr: png_structp; info_ptr: png_infop;
  flag: png_uint_32): png_uint_32; cdecl;
  _png_get_rowbytes: function(png_ptr: png_structp;
  info_ptr: png_infop): png_uint_32; cdecl;
  _png_get_rows: function(png_ptr: png_structp; info_ptr: png_infop): png_bytepp; cdecl;
  _png_set_rows: procedure(png_ptr: png_structp; info_ptr: png_infop;
  row_pointers: png_bytepp); cdecl;
  _png_get_channels: function(png_ptr: png_structp; info_ptr: png_infop): png_byte; cdecl;
  _png_get_image_width: function(png_ptr: png_structp;
  info_ptr: png_infop): png_uint_32; cdecl;
  _png_get_image_height: function(png_ptr: png_structp;
  info_ptr: png_infop): png_uint_32; cdecl;
  _png_get_bit_depth: function(png_ptr: png_structp;
  info_ptr: png_infop): png_byte; cdecl;
  _png_get_color_type: function(png_ptr: png_structp;
  info_ptr: png_infop): png_byte; cdecl;
  _png_get_filter_type: function(png_ptr: png_structp;
  info_ptr: png_infop): png_byte; cdecl;
  _png_get_interlace_type: function(png_ptr: png_structp;
  info_ptr: png_infop): png_byte; cdecl;
  _png_get_compression_type: function(png_ptr: png_structp;
  info_ptr: png_infop): png_byte; cdecl;
  _png_get_pixels_per_meter: function(png_ptr: png_structp;
  info_ptr: png_infop): png_uint_32; cdecl;
  _png_get_x_pixels_per_meter: function(png_ptr: png_structp;
  info_ptr: png_infop): png_uint_32; cdecl;
  _png_get_y_pixels_per_meter: function(png_ptr: png_structp;
  info_ptr: png_infop): png_uint_32; cdecl;
  _png_get_pixel_aspect_ratio: function(png_ptr: png_structp;
  info_ptr: png_infop): double; cdecl;
  _png_get_x_offset_pixels: function(png_ptr: png_structp;
  info_ptr: png_infop): png_int_32; cdecl;
  _png_get_y_offset_pixels: function(png_ptr: png_structp;
  info_ptr: png_infop): png_int_32; cdecl;
  _png_get_x_offset_microns: function(png_ptr: png_structp;
  info_ptr: png_infop): png_int_32; cdecl;
  _png_get_y_offset_microns: function(png_ptr: png_structp;
  info_ptr: png_infop): png_int_32; cdecl;
  _png_get_signature: function(png_ptr: png_structp;
  info_ptr: png_infop): png_bytep; cdecl;
  _png_get_bKGD: function(png_ptr: png_structp; info_ptr: png_infop;
  background: Ppng_color_16p): png_uint_32; cdecl;
  _png_set_bKGD: procedure(png_ptr: png_structp; info_ptr: png_infop;
  background: png_color_16p); cdecl;
  _png_get_cHRM: function(png_ptr: png_structp; info_ptr: png_infop;
  white_x: Pdouble; white_y: Pdouble; red_x: Pdouble; red_y: Pdouble;
  green_x: Pdouble; green_y: Pdouble; blue_x: Pdouble;
  blue_y: Pdouble): png_uint_32; cdecl;
  _png_get_cHRM_fixed: function(png_ptr: png_structp; info_ptr: png_infop;
  int_white_x: Ppng_fixed_point; int_white_y: Ppng_fixed_point;
  int_red_x: Ppng_fixed_point; int_red_y: Ppng_fixed_point;
  int_green_x: Ppng_fixed_point; int_green_y: Ppng_fixed_point;
  int_blue_x: Ppng_fixed_point; int_blue_y: Ppng_fixed_point): png_uint_32; cdecl;
  _png_set_cHRM: procedure(png_ptr: png_structp; info_ptr: png_infop;
  white_x: double; white_y: double; red_x: double; red_y: double;
  green_x: double; green_y: double; blue_x: double; blue_y: double); cdecl;
  _png_set_cHRM_fixed: procedure(png_ptr: png_structp; info_ptr: png_infop;
  int_white_x: png_fixed_point; int_white_y: png_fixed_point;
  int_red_x: png_fixed_point; int_red_y: png_fixed_point;
  int_green_x: png_fixed_point; int_green_y: png_fixed_point;
  int_blue_x: png_fixed_point; int_blue_y: png_fixed_point); cdecl;
  _png_get_gAMA: function(png_ptr: png_structp; info_ptr: png_infop;
  file_gamma: Pdouble): png_uint_32; cdecl;
  _png_get_gAMA_fixed: function(png_ptr: png_structp; info_ptr: png_infop;
  int_file_gamma: Ppng_fixed_point): png_uint_32; cdecl;
  _png_set_gAMA: procedure(png_ptr: png_structp; info_ptr: png_infop;
  file_gamma: double); cdecl;
  _png_set_gAMA_fixed: procedure(png_ptr: png_structp; info_ptr: png_infop;
  int_file_gamma: png_fixed_point); cdecl;
  _png_get_hIST: function(png_ptr: png_structp; info_ptr: png_infop;
  hist: Ppng_uint_16p): png_uint_32; cdecl;
  _png_set_hIST: procedure(png_ptr: png_structp; info_ptr: png_infop;
  hist: png_uint_16p); cdecl;
  _png_get_IHDR: function(png_ptr: png_structp; info_ptr: png_infop;
  Width: Ppng_uint_32; Height: Ppng_uint_32; bit_depth: Plongint;
  color_type: Plongint; interlace_type: Plongint; compression_type: Plongint;
  filter_type: Plongint): png_uint_32; cdecl;
  _png_set_IHDR: procedure(png_ptr: png_structp; info_ptr: png_infop;
  Width: png_uint_32; Height: png_uint_32; bit_depth: longint;
  color_type: longint; interlace_type: longint; compression_type: longint;
  filter_type: longint); cdecl;
  _png_get_oFFs: function(png_ptr: png_structp; info_ptr: png_infop;
  offset_x: Ppng_int_32; offset_y: Ppng_int_32; unit_type: Plongint): png_uint_32; cdecl;
  _png_set_oFFs: procedure(png_ptr: png_structp; info_ptr: png_infop;
  offset_x: png_int_32; offset_y: png_int_32; unit_type: longint); cdecl;
  _png_get_pCAL: function(png_ptr: png_structp; info_ptr: png_infop;
  purpose: Ppng_charp; X0: Ppng_int_32; X1: Ppng_int_32; atype: Plongint;
  nparams: Plongint; units: Ppng_charp; params: Ppng_charpp): png_uint_32; cdecl;
  _png_set_pCAL: procedure(png_ptr: png_structp; info_ptr: png_infop;
  purpose: png_charp; X0: png_int_32; X1: png_int_32; atype: longint;
  nparams: longint; units: png_charp; params: png_charpp); cdecl;
  _png_get_pHYs: function(png_ptr: png_structp; info_ptr: png_infop;
  res_x: Ppng_uint_32; res_y: Ppng_uint_32; unit_type: Plongint): png_uint_32; cdecl;
  _png_set_pHYs: procedure(png_ptr: png_structp; info_ptr: png_infop;
  res_x: png_uint_32; res_y: png_uint_32; unit_type: longint); cdecl;
  _png_get_PLTE: function(png_ptr: png_structp; info_ptr: png_infop;
  palette: Ppng_colorp; num_palette: Plongint): png_uint_32; cdecl;
  _png_set_PLTE: procedure(png_ptr: png_structp; info_ptr: png_infop;
  palette: png_colorp; num_palette: longint); cdecl;
  _png_get_sBIT: function(png_ptr: png_structp; info_ptr: png_infop;
  sig_bit: Ppng_color_8p): png_uint_32; cdecl;
  _png_set_sBIT: procedure(png_ptr: png_structp; info_ptr: png_infop;
  sig_bit: png_color_8p); cdecl;
  _png_get_sRGB: function(png_ptr: png_structp; info_ptr: png_infop;
  intent: Plongint): png_uint_32; cdecl;
  _png_set_sRGB: procedure(png_ptr: png_structp; info_ptr: png_infop;
  intent: longint); cdecl;
  _png_set_sRGB_gAMA_and_cHRM: procedure(png_ptr: png_structp;
  info_ptr: png_infop; intent: longint); cdecl;
  _png_get_iCCP: function(png_ptr: png_structp; info_ptr: png_infop;
  Name: png_charpp; compression_type: Plongint; profile: png_charpp;
  proflen: Ppng_uint_32): png_uint_32; cdecl;
  _png_set_iCCP: procedure(png_ptr: png_structp; info_ptr: png_infop;
  Name: png_charp; compression_type: longint; profile: png_charp;
  proflen: png_uint_32); cdecl;
  _png_get_sPLT: function(png_ptr: png_structp; info_ptr: png_infop;
  entries: png_sPLT_tpp): png_uint_32; cdecl;
  _png_set_sPLT: procedure(png_ptr: png_structp; info_ptr: png_infop;
  entries: png_sPLT_tp; nentries: longint); cdecl;
  _png_get_text: function(png_ptr: png_structp; info_ptr: png_infop;
  text_ptr: Ppng_textp; num_text: Plongint): png_uint_32; cdecl;
  _png_set_text: procedure(png_ptr: png_structp; info_ptr: png_infop;
  text_ptr: png_textp; num_text: longint); cdecl;
  _png_get_tIME: function(png_ptr: png_structp; info_ptr: png_infop;
  mod_time: Ppng_timep): png_uint_32; cdecl;
  _png_set_tIME: procedure(png_ptr: png_structp; info_ptr: png_infop;
  mod_time: png_timep); cdecl;
  _png_get_tRNS: function(png_ptr: png_structp; info_ptr: png_infop;
  trans: Ppng_bytep; num_trans: Plongint;
  trans_values: Ppng_color_16p): png_uint_32; cdecl;
  _png_set_tRNS: procedure(png_ptr: png_structp; info_ptr: png_infop;
  trans: png_bytep; num_trans: longint; trans_values: png_color_16p); cdecl;
  _png_get_sCAL: function(png_ptr: png_structp; info_ptr: png_infop;
  aunit: Plongint; Width: Pdouble; Height: Pdouble): png_uint_32; cdecl;
  _png_set_sCAL: procedure(png_ptr: png_structp; info_ptr: png_infop;
  aunit: longint; Width: double; Height: double); cdecl;
  _png_set_sCAL_s: procedure(png_ptr: png_structp; info_ptr: png_infop;
  aunit: longint; swidth: png_charp; sheight: png_charp); cdecl;
  _png_set_keep_unknown_chunks: procedure(png_ptr: png_structp;
  keep: longint; chunk_list: png_bytep; num_chunks: longint); cdecl;
  _png_set_unknown_chunks: procedure(png_ptr: png_structp; info_ptr: png_infop;
  unknowns: png_unknown_chunkp; num_unknowns: longint); cdecl;
  _png_set_unknown_chunk_location: procedure(png_ptr: png_structp;
  info_ptr: png_infop; chunk: longint; location: longint); cdecl;
  _png_get_unknown_chunks: function(png_ptr: png_structp; info_ptr: png_infop;
  entries: png_unknown_chunkpp): png_uint_32; cdecl;
  _png_set_invalid: procedure(png_ptr: png_structp; info_ptr: png_infop;
  mask: longint); cdecl;
  _png_read_png: procedure(png_ptr: png_structp; info_ptr: png_infop;
  transforms: longint; params: Pointer); cdecl;
  _png_write_png: procedure(png_ptr: png_structp; info_ptr: png_infop;
  transforms: longint; params: Pointer); cdecl;
  _png_get_header_ver: function(png_ptr: png_structp): png_charp; cdecl;
  _png_get_header_version: function(png_ptr: png_structp): png_charp; cdecl;
  _png_get_libpng_ver: function(png_ptr: png_structp): png_charp; cdecl;

implementation



const
  INVALID_MODULEHANDLE = 0;

var
   {$IFDEF MSWINDOWS}
  vzHandle: HINST;
   {$ENDIF}
   {$IFDEF UNIX}
  vzHandle: TLibHandle = 0;

   {$ENDIF}

function Loadlibpng13: boolean;
begin
  Result := False;
  if (vzHandle = INVALID_MODULEHANDLE) then
  begin
    Closezlib;
    vzHandle := LoadLibrary(PChar(LibPng13));
    if (vzHandle <> INVALID_MODULEHANDLE) then
      Result := True
    else
    begin
      if vzHandle <> INVALID_MODULEHANDLE then
        FreeLibrary(vzHandle);
      {$IFDEF GLS_GLS_LOGGING}
      GLSLogger.Log('LIBPNG.pas: libpng13 library not loaded');
      {$ENDIF}
    end;
  end
  else
    Result := True;
end;

procedure Unloadlibpng13;
begin
  if vzHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(vzHandle);
    vzHandle := INVALID_MODULEHANDLE;
  end;
end;

function GetAddress(ProcName: PChar):Pointer;
begin
  result := GetProcAddress(vzHandle, ProcName);
end;

procedure ReadEntryPoints;
begin
  _png_access_version_number := GetAddress('png_access_version_number');
  _png_set_sig_bytes := GetAddress('png_set_sig_bytes');
  _png_sig_cmp := GetAddress('png_sig_cmp');
  _png_check_sig := GetAddress('png_check_sig');
  _png_create_read_struct := GetAddress('png_create_read_struct');
  _png_create_write_struct := GetAddress('png_create_write_struct');
  _png_get_compression_buffer_size := GetAddress('png_get_compression_buffer_size');
  _png_set_compression_buffer_size := GetAddress('png_set_compression_buffer_size');
  _png_reset_zstream := GetAddress('png_reset_zstream');
  _png_write_chunk := GetAddress('png_write_chunk');
  _png_write_chunk_start := GetAddress('png_write_chunk_start');
  _png_write_chunk_data := GetAddress('png_write_chunk_data');
  _png_write_chunk_end := GetAddress('png_write_chunk_end');
  _png_create_info_struct := GetAddress('png_create_info_struct');
  _png_info_init := GetAddress('png_info_init');
  _png_write_info_before_PLTE := GetAddress('png_write_info_before_PLTE');
  _png_write_info := GetAddress('png_write_info');
  _png_read_info := GetAddress('png_read_info');
  _png_convert_to_rfc1123 := GetAddress('png_convert_to_rfc1123');
  _png_convert_from_struct_tm := GetAddress('png_convert_from_struct_tm');
  _png_convert_from_time_t := GetAddress('png_convert_from_time_t');
  _png_set_expand := GetAddress('png_set_expand');
  _png_set_gray_1_2_4_to_8 := GetAddress('png_set_gray_1_2_4_to_8');
  _png_set_palette_to_rgb := GetAddress('png_set_palette_to_rgb');
  _png_set_tRNS_to_alpha := GetAddress('png_set_tRNS_to_alpha');
  _png_set_bgr := GetAddress('png_set_bgr');
  _png_set_gray_to_rgb := GetAddress('png_set_gray_to_rgb');
  _png_set_rgb_to_gray := GetAddress('png_set_rgb_to_gray');
  _png_set_rgb_to_gray_fixed := GetAddress('png_set_rgb_to_gray_fixed');
  _png_get_rgb_to_gray_status := GetAddress('png_get_rgb_to_gray_status');
  _png_build_grayscale_palette := GetAddress('png_build_grayscale_palette');
  _png_set_strip_alpha := GetAddress('png_set_strip_alpha');
  _png_set_swap_alpha := GetAddress('png_set_swap_alpha');
  _png_set_invert_alpha := GetAddress('png_set_invert_alpha');
  _png_set_filler := GetAddress('png_set_filler');
  _png_set_swap := GetAddress('png_set_swap');
  _png_set_packing := GetAddress('png_set_packing');
  _png_set_packswap := GetAddress('png_set_packswap');
  _png_set_shift := GetAddress('png_set_shift');
  _png_set_interlace_handling := GetAddress('png_set_interlace_handling');
  _png_set_invert_mono := GetAddress('png_set_invert_mono');
  _png_set_background := GetAddress('png_set_background');
  _png_set_strip_16 := GetAddress('png_set_strip_16');
  _png_set_dither := GetAddress('png_set_dither');
  _png_set_gamma := GetAddress('png_set_gamma');
  _png_permit_empty_plte := GetAddress('png_permit_empty_plte');
  _png_set_flush := GetAddress('png_set_flush');
  _png_write_flush := GetAddress('png_write_flush');
  _png_start_read_image := GetAddress('png_start_read_image');
  _png_read_update_info := GetAddress('png_read_update_info');
  _png_read_rows := GetAddress('png_read_rows');
  _png_read_row := GetAddress('png_read_row');
  _png_read_image := GetAddress('png_read_image');
  _png_write_row := GetAddress('png_write_row');
  _png_write_rows := GetAddress('png_write_rows');
  _png_write_image := GetAddress('png_write_image');
  _png_write_end := GetAddress('png_write_end');
  _png_read_end := GetAddress('png_read_end');
  _png_destroy_info_struct := GetAddress('png_destroy_info_struct');
  _png_destroy_read_struct := GetAddress('png_destroy_read_struct');
  _png_read_destroy := GetAddress('png_read_destroy');
  _png_destroy_write_struct := GetAddress('png_destroy_write_struct');
  _png_write_destroy_info := GetAddress('png_write_destroy_info');
  _png_write_destroy := GetAddress('png_write_destroy');
  _png_set_crc_action := GetAddress('png_set_crc_action');
  _png_set_filter := GetAddress('png_set_filter');
  _png_set_filter_heuristics := GetAddress('png_set_filter_heuristics');
  _png_set_compression_level := GetAddress('png_set_compression_level');
  _png_set_compression_mem_level := GetAddress('png_set_compression_mem_level');
  _png_set_compression_strategy := GetAddress('png_set_compression_strategy');
  _png_set_compression_window_bits := GetAddress('png_set_compression_window_bits');
  _png_set_compression_method := GetAddress('png_set_compression_method');
  _png_init_io := GetAddress('png_init_io');
  _png_set_error_fn := GetAddress('png_set_error_fn');
  _png_get_error_ptr := GetAddress('png_get_error_ptr');
  _png_set_write_fn := GetAddress('png_set_write_fn');
  _png_set_read_fn := GetAddress('png_set_read_fn');
  _png_get_io_ptr := GetAddress('png_get_io_ptr');
  _png_set_read_status_fn := GetAddress('png_set_read_status_fn');
  _png_set_write_status_fn := GetAddress('png_set_write_status_fn');
  _png_set_read_user_transform_fn := GetAddress('png_set_read_user_transform_fn');
  _png_set_write_user_transform_fn := GetAddress('png_set_write_user_transform_fn');
  _png_set_user_transform_info := GetAddress('png_set_user_transform_info');
  _png_get_user_transform_ptr := GetAddress('png_get_user_transform_ptr');
  _png_set_read_user_chunk_fn := GetAddress('png_set_read_user_chunk_fn');
  _png_get_user_chunk_ptr := GetAddress('png_get_user_chunk_ptr');
  _png_set_progressive_read_fn := GetAddress('png_set_progressive_read_fn');
  _png_get_progressive_ptr := GetAddress('png_get_progressive_ptr');
  _png_process_data := GetAddress('png_process_data');
  _png_progressive_combine_row := GetAddress('png_progressive_combine_row');
  _png_malloc := GetAddress('png_malloc');
  _png_free := GetAddress('png_free');
  _png_free_data := GetAddress('png_free_data');
  _png_data_freer := GetAddress('png_data_freer');
  _png_memcpy_check := GetAddress('png_memcpy_check');
  _png_memset_check := GetAddress('png_memset_check');
  _png_error := GetAddress('png_error');
  _png_chunk_error := GetAddress('png_chunk_error');
  _png_warning := GetAddress('png_warning');
  _png_chunk_warning := GetAddress('png_chunk_warning');
  _png_get_valid := GetAddress('png_get_valid');
  _png_get_rowbytes := GetAddress('png_get_rowbytes');
  _png_get_rows := GetAddress('png_get_rows');
  _png_set_rows := GetAddress('png_set_rows');
  _png_get_channels := GetAddress('png_get_channels');
  _png_get_image_width := GetAddress('png_get_image_width');
  _png_get_image_height := GetAddress('png_get_image_height');
  _png_get_bit_depth := GetAddress('png_get_bit_depth');
  _png_get_color_type := GetAddress('png_get_color_type');
  _png_get_filter_type := GetAddress('png_get_filter_type');
  _png_get_interlace_type := GetAddress('png_get_interlace_type');
  _png_get_compression_type := GetAddress('png_get_compression_type');
  _png_get_pixels_per_meter := GetAddress('png_get_pixels_per_meter');
  _png_get_x_pixels_per_meter := GetAddress('png_get_x_pixels_per_meter');
  _png_get_y_pixels_per_meter := GetAddress('png_get_y_pixels_per_meter');
  _png_get_pixel_aspect_ratio := GetAddress('png_get_pixel_aspect_ratio');
  _png_get_x_offset_pixels := GetAddress('png_get_x_offset_pixels');
  _png_get_y_offset_pixels := GetAddress('png_get_y_offset_pixels');
  _png_get_x_offset_microns := GetAddress('png_get_x_offset_microns');
  _png_get_y_offset_microns := GetAddress('png_get_y_offset_microns');
  _png_get_signature := GetAddress('png_get_signature');
  _png_get_bKGD := GetAddress('png_get_bKGD');
  _png_set_bKGD := GetAddress('png_set_bKGD');
  _png_get_cHRM := GetAddress('png_get_cHRM');
  _png_get_cHRM_fixed := GetAddress('png_get_cHRM_fixed');
  _png_set_cHRM := GetAddress('png_set_cHRM');
  _png_set_cHRM_fixed := GetAddress('png_set_cHRM_fixed');

  _png_get_gAMA := GetAddress('png_get_gAMA');
  _png_get_gAMA_fixed := GetAddress('png_get_gAMA_fixed');
  _png_set_gAMA := GetAddress('png_set_gAMA');
  _png_set_gAMA_fixed := GetAddress('png_set_gAMA_fixed');
  _png_get_hIST := GetAddress('png_get_hIST');
  _png_set_hIST := GetAddress('png_set_hIST');
  _png_get_IHDR := GetAddress('png_get_IHDR');
  _png_set_IHDR := GetAddress('png_set_IHDR');
  _png_get_oFFs := GetAddress('png_get_oFFs');
  _png_set_oFFs := GetAddress('png_set_oFFs');
  _png_get_pCAL := GetAddress('png_get_pCAL');
  _png_set_pCAL := GetAddress('png_set_pCAL');
  _png_get_pHYs := GetAddress('png_get_pHYs');
  _png_set_pHYs := GetAddress('png_set_pHYs');
  _png_get_PLTE := GetAddress('png_get_PLTE');
  _png_set_PLTE := GetAddress('png_set_PLTE');
  _png_get_sBIT := GetAddress('png_get_sBIT');
  _png_set_sBIT := GetAddress('png_set_sBIT');
  _png_get_sRGB := GetAddress('png_get_sRGB');
  _png_set_sRGB := GetAddress('png_set_sRGB');
  _png_set_sRGB_gAMA_and_cHRM := GetAddress('png_set_sRGB_gAMA_and_cHRM');
  _png_get_iCCP := GetAddress('png_get_iCCP');
  _png_set_iCCP := GetAddress('png_set_iCCP');
  _png_get_sPLT := GetAddress('png_get_sPLT');
  _png_set_sPLT := GetAddress('png_set_sPLT');
  _png_get_text := GetAddress('png_get_text');
  _png_set_text := GetAddress('png_set_text');
  _png_get_tIME := GetAddress('png_get_tIME');
  _png_set_tIME := GetAddress('png_set_tIME');
  _png_get_tRNS := GetAddress('png_get_tRNS');
  _png_set_tRNS := GetAddress('png_set_tRNS');
  _png_get_sCAL := GetAddress('png_get_sCAL');
  _png_set_sCAL := GetAddress('png_set_sCAL');
  _png_set_sCAL_s := GetAddress('png_set_sCAL_s');
  _png_set_keep_unknown_chunks := GetAddress('png_set_keep_unknown_chunks');
  _png_set_unknown_chunks := GetAddress('png_set_unknown_chunks');
  _png_set_unknown_chunk_location := GetAddress('png_set_unknown_chunk_location');
  _png_get_unknown_chunks := GetAddress('png_get_unknown_chunks');
  _png_set_invalid := GetAddress('png_set_invalid');
  _png_read_png := GetAddress('png_read_png');
  _png_write_png := GetAddress('png_write_png');
  _png_get_header_ver := GetAddress('png_get_header_ver');
  _png_get_header_version := GetAddress('png_get_header_version');
  _png_get_libpng_ver := GetAddress('png_get_libpng_ver');
end;


procedure pngReadFn(png_ptr: png_structp; Data: png_bytep; length: png_size_t);
var
  fs: TStream;
begin
  fs := TStream(_png_get_io_ptr(png_ptr));
  Assert(Assigned(Data), 'Attempt to read from null file pointer');
  fs.Read(Data^, length);
end;

procedure pngWriteFn(png_ptr: png_structp; Data: png_bytep; length: png_size_t);
var
  fs: TStream;
begin
  fs := TStream(_png_get_io_ptr(png_ptr));
  Assert(Assigned(Data), 'Attempt to write to null file pointer');
  fs.Write(Data^, length);
end;

procedure pngErrorFn(struct: png_structp; str: png_const_charp); cdecl;
begin
{$IFDEF GLS_LOGGING}
  GLSLogger.Log(string(str), lkError);
{$ENDIF}
end;

procedure pngWarnFn(struct: png_structp; str: png_const_charp); cdecl;
begin
{$IFDEF GLS_LOGGING}
  GLSLogger.Log(string(str), lkWarning);
{$ENDIF}
end;

initialization


  if Loadlibpng13 then ReadEntryPoints;


finalization

  Unloadlibpng13;


end.
