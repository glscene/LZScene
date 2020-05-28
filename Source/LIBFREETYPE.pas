//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   History :  
       21/01/11 - Yar - Creation
   
}

unit LIBFREETYPE;

interface

{$I GLScene.inc}

uses
{$IFDEF GLS_LOGGING} GLSLog, {$ENDIF}
{$IFDEF MSWINDOWS} Windows, {$ENDIF}
{$IFDEF Unix} x, dynlibs, {$ENDIF}
 GLCrossPlatform;

type
  FT_Encoding = array[0..3] of char;

const
{$IFDEF MSWINDOWS}
  FTLIBNAME = 'freetype.dll';
{$ENDIF }

{$IFDEF LINUX }
  FTLIBNAME = 'freetype.so';
{$ENDIF }

{$IFDEF DARWIN }
  FTLIBNAME = 'FTLIBNAME';
{$ENDIF }

  FT_CURVE_TAG_ON = 1;
  FT_CURVE_TAG_CONIC = 0;
  FT_CURVE_TAG_CUBIC = 2;

  FT_FACE_FLAG_SCALABLE = 1 shl 0;
  FT_FACE_FLAG_KERNING = 1 shl 6;

  FT_ENCODING_NONE: FT_Encoding = (#0, #0, #0, #0);

  FT_LOAD_DEFAULT = $0000;
  FT_LOAD_NO_HINTING = $0002;
  FT_LOAD_FORCE_AUTOHINT = $0020;

  FT_RENDER_MODE_NORMAL = 0;
  FT_RENDER_MODE_LIGHT = FT_RENDER_MODE_NORMAL + 1;
  FT_RENDER_MODE_MONO = FT_RENDER_MODE_LIGHT + 1;
  FT_RENDER_MODE_LCD = FT_RENDER_MODE_MONO + 1;
  FT_RENDER_MODE_LCD_V = FT_RENDER_MODE_LCD + 1;
  FT_RENDER_MODE_MAX = FT_RENDER_MODE_LCD_V + 1;

  FT_KERNING_DEFAULT = 0;
  FT_KERNING_UNFITTED = 1;
  FT_KERNING_UNSCALED = 2;

  FT_STYLE_FLAG_ITALIC = 1 shl 0;
  FT_STYLE_FLAG_BOLD = 1 shl 1;

  T1_MAX_MM_AXIS = 4;

  FT_VALIDATE_feat_INDEX = 0;
  FT_VALIDATE_mort_INDEX = 1;
  FT_VALIDATE_morx_INDEX = 2;
  FT_VALIDATE_bsln_INDEX = 3;
  FT_VALIDATE_just_INDEX = 4;
  FT_VALIDATE_kern_INDEX = 5;
  FT_VALIDATE_opbd_INDEX = 6;
  FT_VALIDATE_trak_INDEX = 7;
  FT_VALIDATE_prop_INDEX = 8;
  FT_VALIDATE_lcar_INDEX = 9;
  FT_VALIDATE_GX_LAST_INDEX = FT_VALIDATE_lcar_INDEX;
  FT_VALIDATE_GX_LENGTH = FT_VALIDATE_GX_LAST_INDEX + 1;

  FT_OPEN_MEMORY    = $1;
  FT_OPEN_STREAM    = $2;
  FT_OPEN_PATHNAME  = $4;
  FT_OPEN_DRIVER    = $8;
  FT_OPEN_PARAMS    = $10;

  FT_OUTLINE_NONE             = $0;
  FT_OUTLINE_OWNER            = $1;
  FT_OUTLINE_EVEN_ODD_FILL    = $2;
  FT_OUTLINE_REVERSE_FILL     = $4;
  FT_OUTLINE_IGNORE_DROPOUTS  = $8;

  FT_OUTLINE_HIGH_PRECISION   = $100;
  FT_OUTLINE_SINGLE_PASS      = $200;

type
  FT_Pointer = pointer;
  FT_Byte = byte;
  FT_Byte_array = array[0..MaxInt div (2*SizeOf(FT_Byte))-1] of FT_Byte;
  FT_Bytes = ^FT_Byte_array;
  FT_Short = smallint;
  FT_UShort = word;
  FT_Int = longint;
  FT_UInt = longword;
  FT_Int32 = longint;
  FT_UInt32 = cardinal;
  FT_Long = longint;
  FT_ULong = longword;
  FT_Fixed = longint;
  FT_Bool = bytebool;
  FT_Char = ShortInt;
  FT_Pos = longint;
  FT_Error = longint;
  FT_F26Dot6 = longint;
  FT_String = AnsiChar;

  FT_Matrix = packed record
    xx, xy: FT_Fixed;
    yx, yy: FT_Fixed;
  end;

  FT_Byte_ptr = ^FT_Byte;
  FT_Short_array = array[0..MaxInt div (2*SizeOf(FT_Short))-1] of FT_Short;
  FT_Short_ptr = ^FT_Short_array;
  FT_Long_ptr = ^FT_Long;
  FT_Fixed_ptr = ^FT_Fixed;
  FT_UInt_ptr = ^FT_UInt;

  FT_Render_Mode = FT_Int;


  FT_Library = ^FT_LibraryRec_;
  FT_LibraryRec_ = record
  end;
  FT_Library_array = array[0..MaxInt div 16 - 1] of FT_Library;
  FT_Library_ptr = ^FT_Library_array;

  FT_Subglyph_ptr = ^FT_Subglyph;
  FT_Subglyph = record
  end;

  FT_Bitmap_Size = packed record
    height, width: FT_Short;
  end;

  AFT_Bitmap_Size = array[0..1023] of FT_Bitmap_Size;
  FT_Bitmap_Size_ptr = ^AFT_Bitmap_Size;



  FT_Generic_Finalizer = procedure(AnObject: pointer); cdecl;

  FT_Generic = packed record
    data: pointer;
    finalizer: FT_Generic_Finalizer;
  end;

  FT_BBox_ptr = ^FT_BBox;
  FT_BBox = packed record
    xMin, yMin, xMax, yMax: FT_Pos;
  end;

  FT_Vector_ptr = ^FT_Vector_array;
  FT_Vector = packed record
    x, y: FT_Pos;
  end;
  FT_Vector_array = array[0..MaxInt div (2*SizeOf(FT_Vector)) - 1] of FT_Vector;

  FT_Bitmap_ptr = ^FT_Bitmap;
  FT_Bitmap = packed record
    rows, width, pitch: FT_Int;
    buffer: pointer;
    num_grays: FT_Short;
    pixel_mode, palette_mode: char;
    palette: pointer;
  end;

  FT_Outline = ^FT_OutlineRec_;
  FT_OutlineRec_ = packed record
    n_contours, n_points: FT_Short;
    points: FT_Vector_ptr;
    tags: PChar;
    contours: FT_Short_ptr;
    flags: FT_Int;
  end;

  FT_Glyph_Metrics = packed record
    width, height,
      horiBearingX,
      horiBearingY,
      horiAdvance,
      vertBearingX,
      vertBearingY,
      vertAdvance: FT_Pos;
  end;

  FT_Face = ^FT_FaceRec_;
  FT_Face_array = array[0..MaxInt div 16 - 1] of FT_Face;
  FT_Face_ptr = ^FT_Face_array;

  FT_GlyphSlot = ^FT_GlyphSlotRec_;
  FT_GlyphSlotRec_ = packed record
    alibrary: FT_Library_ptr;

    face: FT_Face_ptr;
    next: FT_GlyphSlot;
    flags: FT_UInt;

    generic: FT_Generic;
    metrics: FT_Glyph_Metrics;

    linearHoriAdvance: FT_Fixed;
    linearVertAdvance: FT_Fixed;

    advance: FT_Vector;
    format: longword;
    bitmap: FT_Bitmap;

    bitmap_left: FT_Int;
    bitmap_top: FT_Int;

    outline: FT_Outline;

    num_subglyphs: FT_UInt;
    subglyphs: FT_SubGlyph_ptr;
    control_data: pointer;
    control_len: longint;

    other: pointer;

  end;

  FT_Size_Metrics = record
    x_ppem, y_ppem: FT_UShort;
    x_scale, y_scale: FT_Fixed;
    ascender,
      descender,
      height,
      max_advance: FT_Pos;
  end;

  FT_Size = ^FT_SizeRec_;
  FT_SizeRec_ = record
    face: FT_Face_ptr;
    generic: FT_Generic;
    metrics: FT_Size_Metrics;
    //internal : FT_Size_Internal;
  end;

  FT_Charmap = ^FT_CharmapRec_;
  FT_Charmap_ptr = ^FT_Charmap_array;

  FT_FaceRec_ = packed record
    num_faces,
      face_index,
      face_flags,
      style_flags,
      num_glyphs: FT_Long;
    family_name,
      style_name: PChar;

    num_fixed_sizes: FT_Int;
    available_sizes: FT_Bitmap_Size_ptr; // is array

    num_charmaps: FT_Int;
    charmaps: FT_CharMap_ptr; // is array

    generic: FT_Generic;
    bbox: FT_BBox;

    units_per_EM: FT_UShort;

    ascender,
      descender,
      height,

    max_advance_width,
      max_advance_height,
      underline_position,
      underline_thickness: FT_Short;

    glyph: FT_GlyphSlot;
    size: FT_Size;
    charmap: FT_CharMap;

  end;

  FT_CharmapRec_ = packed record
    face: FT_Face_ptr;
    encoding: FT_Encoding;
    platform_id,
      encoding_id: FT_UShort;
  end;
  FT_Charmap_array = array[0..MaxInt div 16 - 1] of FT_Charmap;


  FTC_CMapCache = ^FTC_CMapCacheRec_;
  FTC_CMapCacheRec_ = record
  end;

  FTC_FaceID = FT_Pointer;

  FTC_Manager = ^FTC_ManagerRec_;
  FTC_ManagerRec_ = record
  end;

  FTC_ImageCache = ^FTC_ImageCacheRec_;
  FTC_ImageCacheRec_ = record
  end;

  FTC_ImageType = ^FTC_ImageTypeRec_;
  FTC_ImageTypeRec_ = packed record
    face_id: FTC_FaceID;
    width: FT_Int;
    height: FT_Int;
    flags: FT_Int32;
  end;

  FT_Glyph = ^FT_GlyphRec_;
  FT_GlyphRec_ = record
  end;

  FTC_Node = ^FTC_NodeRec_;
  FTC_NodeRec_ = record
  end;

  FTC_Scaler = ^FTC_ScalerRec_;
  FTC_ScalerRec_ = packed record
    face_id: FTC_FaceID;
    width: FT_UInt;
    height: FT_UInt;
    pixel: FT_Int;
    x_res: FT_UInt;
    y_res: FT_UInt;
  end;

  FT_Angle = FT_Fixed;

  FTC_Face_Requester = function(face_id: FTC_FaceID;
    Alibrary: FT_Library;
    request_data: FT_Pointer;
    var aface: FT_Face): FT_Error; cdecl;

  FTC_SBitCache = ^FTC_SBitCacheRec_;
  FTC_SBitCacheRec_ = record
  end;

  FTC_SBit = ^FTC_SBitRec_;
  FTC_SBitRec_ = packed record
    width: FT_Byte;
    height: FT_Byte;
    left: FT_Char;
    top: FT_Char;
    format: FT_Byte;
    max_grays: FT_Byte;
    pitch: FT_Short;
    xadvance: FT_Char;
    yadvance: FT_Char;
    buffer: ^FT_Byte;
  end;

  FT_Module_Class = FT_Pointer;

  FT_Module = ^FT_ModuleRec_;
  FT_ModuleRec_ = record
  end;

  FT_MM_Axis = packed record
    name: ^FT_String;
    minimum: FT_Long;
    maximum: FT_Long;
  end;

  FT_Multi_Master = packed record
    num_axis: FT_UInt;
    num_designs: FT_UInt;
    axis: array[0..T1_MAX_MM_AXIS - 1] of FT_MM_Axis;
  end;

  PS_FontInfo = ^PS_FontInfoRec_;
  PS_FontInfoRec_ = packed record
    version: ^FT_String;
    notice: ^FT_String;
    full_name: ^FT_String;
    family_name: ^FT_String;
    weight: ^FT_String;
    italic_angle: FT_Long;
    is_fixed_pitch: FT_Bool;
    underline_position: FT_Short;
    underline_thickness: FT_UShort;
  end;

  PS_Private = ^PS_PrivateRec_;
  PS_PrivateRec_ = packed record
    unique_id: FT_Int;
    lenIV: FT_Int;

    num_blue_values: FT_Byte;
    num_other_blues: FT_Byte;
    num_family_blues: FT_Byte;
    num_family_other_blues: FT_Byte;

    blue_values: array[0..13] of FT_Short;
    other_blues: array[0..9] of FT_Short;

    family_blues: array[0..13] of FT_Short;
    family_other_blues: array[0..9] of FT_Short;

    blue_scale: FT_Fixed;
    blue_shift: FT_Int;
    blue_fuzz: FT_Int;

    standard_width: array[0..1] of FT_UShort;
    standard_height: array[0..1] of FT_UShort;

    num_snap_widths: FT_Byte;
    num_snap_heights: FT_Byte;
    force_bold: FT_Byte;
    round_stem_up: FT_Byte;

    snap_widths: array[0..13] of FT_Short;
    snap_heights: array[0..13] of FT_Short;

    expansion_factor: FT_Fixed;

    language_group: FT_Long;
    password: FT_Long;

    min_feature: array[0..1] of FT_Short;
  end;

  FT_Glyph_Format =
    (
    FT_GLYPH_FORMAT_NONE = $00000000,
    FT_GLYPH_FORMAT_COMPOSITE = $706D6F63,
    FT_GLYPH_FORMAT_BITMAP = $73746962,
    FT_GLYPH_FORMAT_OUTLINE = $6C74756F,
    FT_GLYPH_FORMAT_PLOTTER = $746F6C70
    );

  FT_Glyph_Class = ^FT_Glyph_ClassRec_;
  FT_Glyph_ClassRec_ = packed record
  end;

  FT_Raster = ^FT_RasterRec_;
  FT_RasterRec_ = packed record
  end;

  pFT_Raster_Params = ^FT_Raster_Params;

  FT_Raster_Render_Func = function(raster: FT_Raster;
    params: pFT_Raster_Params): Integer; cdecl;

  FT_Renderer = ^FT_RendererRec_;

  FT_Renderer_RenderFunc = function(renderer: FT_Renderer;
    slot: FT_GlyphSlot;
    mode: FT_UInt;
    const origin: FT_Vector): FT_Error; cdecl;

  FT_Renderer_TransformFunc = function(renderer: FT_Renderer;
    slot: FT_GlyphSlot;
    const matrix: FT_Matrix;
    const delta: FT_Vector): FT_Error; cdecl;

  FT_Renderer_GetCBoxFunc = procedure(renderer: FT_Renderer;
    slot: FT_GlyphSlot;
    var cbox: FT_BBox); cdecl;

  FT_Renderer_SetModeFunc = function(renderer: FT_Renderer;
    mode_tag: FT_ULong;
    mode_ptr: FT_Pointer): FT_Error; cdecl;

  FT_Raster_NewFunc = function(memory: Pointer;
    var raster: FT_Raster): Integer; cdecl;

  FT_Raster_ResetFunc = procedure(raster: FT_Raster;
    pool_base: PAnsiChar;
    pool_size: PtrUint); cdecl;

  FT_Raster_SetModeFunc = function(raster: FT_Raster;
    mode: PtrUint;
    args: Pointer): Integer; cdecl;

  FT_Raster_RenderFunc = function(raster: FT_Raster;
    params: pFT_Raster_Params): Integer; cdecl;

  FT_Raster_DoneFunc = procedure(raster: FT_Raster); cdecl;

  FT_Raster_Funcs = packed record
    glyph_format: FT_Glyph_Format;
    raster_new: FT_Raster_NewFunc;
    raster_reset: FT_Raster_ResetFunc;
    raster_set_mode: FT_Raster_SetModeFunc;
    raster_render: FT_Raster_RenderFunc;
    raster_done: FT_Raster_DoneFunc;
  end;

  FT_Renderer_Class = packed record
    root: FT_Module_Class;
    glyph_format: FT_Glyph_Format;
    render_glyph: FT_Renderer_RenderFunc;
    transform_glyph: FT_Renderer_TransformFunc;
    get_glyph_cbox: FT_Renderer_GetCBoxFunc;
    set_mode: FT_Renderer_SetModeFunc;
    raster_class: ^FT_Raster_Funcs;
  end;

  FT_RendererRec_ = packed record
    root: FT_ModuleRec_;
    clazz: ^FT_Renderer_Class;
    glyph_format: FT_Glyph_Format;
    glyph_class: FT_Glyph_Class;

    raster: FT_Raster;
    raster_render: FT_Raster_Render_Func;
    render: FT_Renderer_RenderFunc;
  end;

  FT_Memory = ^FT_MemoryRec_;

  FT_Alloc_Func = function(memory: FT_Memory;
    size: LongInt): Pointer;

  FT_Free_Func = procedure(memory: FT_Memory;
    block: Pointer); cdecl;

  FT_Realloc_Func = function(memory: FT_Memory;
    cur_size: LongInt;
    new_size: LongInt;
    block: Pointer): Pointer; cdecl;

  FT_MemoryRec_ = packed record
    user: Pointer;
    alloc: FT_Alloc_Func;
    free: FT_Free_Func;
    realloc: FT_Realloc_Func;
  end;

  FT_SfntName = packed record
    platform_id: FT_UShort;
    encoding_id: FT_UShort;
    language_id: FT_UShort;
    name_id: FT_UShort;

    AString: ^FT_Byte; // this string is *not* null-terminated!
    string_len: FT_UInt; // in bytes
  end;

  FT_Sfnt_Tag =
    (
    ft_sfnt_head = 0, // TT_Header
    ft_sfnt_maxp = 1, // TT_MaxProfile
    ft_sfnt_os2 = 2, // TT_OS2
    ft_sfnt_hhea = 3, // TT_HoriHeader
    ft_sfnt_vhea = 4, // TT_VertHeader
    ft_sfnt_post = 5, // TT_Postscript
    ft_sfnt_pclt = 6
    );

  FT_TrueTypeEngineType =
    (
    FT_TRUETYPE_ENGINE_TYPE_NONE,
    FT_TRUETYPE_ENGINE_TYPE_UNPATENTED,
    FT_TRUETYPE_ENGINE_TYPE_PATENTE
    );

  FT_WinFNT_HeaderRec = packed record
    version: FT_UShort;
    file_size: FT_ULong;
    copyright: array[0..59] of FT_Byte;
    file_type: FT_UShort;
    nominal_point_size: FT_UShort;
    vertical_resolution: FT_UShort;
    horizontal_resolution: FT_UShort;
    ascent: FT_UShort;
    internal_leading: FT_UShort;
    external_leading: FT_UShort;
    italic: FT_Byte;
    underline: FT_Byte;
    strike_out: FT_Byte;
    weight: FT_UShort;
    charset: FT_Byte;
    pixel_width: FT_UShort;
    pixel_height: FT_UShort;
    pitch_and_family: FT_Byte;
    avg_width: FT_UShort;
    max_width: FT_UShort;
    first_char: FT_Byte;
    last_char: FT_Byte;
    default_char: FT_Byte;
    break_char: FT_Byte;
    bytes_per_row: FT_UShort;
    device_offset: FT_ULong;
    face_name_offset: FT_ULong;
    bits_pointer: FT_ULong;
    bits_offset: FT_ULong;
    reserved: FT_Byte;
    flags: FT_ULong;
    A_space: FT_UShort;
    B_space: FT_UShort;
    C_space: FT_UShort;
    color_table_offset: FT_UShort;
    reserved1: array[0..3] of FT_ULong;
  end;

  FT_Stroker = ^FT_StrokerRec_;
  FT_StrokerRec_ = packed record
  end;

  FT_ListNode = ^FT_ListNodeRec_;
  FT_ListNodeRec_ = packed record
    prev: FT_ListNode;
    next: FT_ListNode;
    data: Pointer;
  end;

  FT_List = ^FT_ListRec_;
  FT_ListRec_ = packed record
    head: FT_ListNode;
    tail: FT_ListNode;
  end;

  FT_List_Destructor = procedure(memory: FT_Memory;
    data: Pointer;
    user: Pointer); cdecl;

  FT_List_Iterator = function(node: FT_ListNode;
    user: Pointer): FT_Error; cdecl;

  FT_StrokerBorder =
    (
    FT_STROKER_BORDER_LEFT,
    FT_STROKER_BORDER_RIGHT
    );

  FT_Orientation =
    (
    FT_ORIENTATION_TRUETYPE = 0,
    FT_ORIENTATION_POSTSCRIPT = 1,
    FT_ORIENTATION_FILL_RIGHT = FT_ORIENTATION_TRUETYPE,
    FT_ORIENTATION_FILL_LEFT = FT_ORIENTATION_POSTSCRIPT,
    FT_ORIENTATION_NONE = 3
    );

  FT_Span = packed record
    x: SmallInt;
    len: Word;
    coverage: Byte;
  end;

  FT_SpanFunc = procedure(y: Integer;
    count: Integer;
    var spans: FT_Span;
    user: Pointer); cdecl;

  FT_Raster_BitTest_Func = function(y: Integer;
    x: Integer;
    user: Pointer): Integer; cdecl;

  FT_Raster_BitSet_Func = procedure(y: Integer;
    x: Integer;
    user: Pointer); cdecl;

  FT_Raster_Params = packed record
    target: ^FT_Bitmap;
    source: Pointer;
    flags: Integer;
    gray_spans: FT_SpanFunc;
    black_spans: FT_SpanFunc; // doesn't work!
    bit_test: FT_Raster_BitTest_Func; // doesn't work!
    bit_set: FT_Raster_BitSet_Func; // doesn't work!
    user: Pointer;
    clip_box: FT_BBox;
  end;

  FT_Size_Request_Type =
    (
    FT_SIZE_REQUEST_TYPE_NOMINAL,
    FT_SIZE_REQUEST_TYPE_REAL_DIM,
    FT_SIZE_REQUEST_TYPE_BBOX,
    FT_SIZE_REQUEST_TYPE_CELL,
    FT_SIZE_REQUEST_TYPE_SCALES,

    FT_SIZE_REQUEST_TYPE_MAX
    );

  FT_Size_Request = ^FT_Size_RequestRec_;
  FT_Size_RequestRec_ = packed record
    atype: FT_Size_Request_Type;
    width: FT_Long;
    height: FT_Long;
    horiResolution: FT_UInt;
    vertResolution: FT_UInt;
  end;

  FT_DebugHook_Func = procedure(arg: Pointer); cdecl;

  FT_StreamDesc = packed record
    case Byte of
      0: (Val: record Value: PtrUint;
        end);
      1: (Ptr: record Value: Pointer;
        end);
  end;

  FT_Stream = ^FT_StreamRec_;

  FT_Stream_IoFunc = function(stream: FT_Stream;
    offset: PtrUint;
    buffer: PAnsiChar;
    count: PtrUint): PtrUint; cdecl;

  FT_Stream_CloseFunc = procedure(stream: FT_Stream); cdecl;

  FT_StreamRec_ = packed record
    base: PAnsiChar;
    size: PtrUint;
    pos: PtrUint;

    descriptor: FT_StreamDesc;
    pathname: FT_StreamDesc;
    read: FT_Stream_IoFunc;
    close: FT_Stream_CloseFunc;

    memory: FT_Memory;
    cursor: PAnsiChar;
    limit: PAnsiChar;
  end;

  FT_Parameter = packed record
    tag: FT_ULong;
    data: FT_Pointer;
  end;

  FT_Stroker_LineCap =
    (
    FT_STROKER_LINECAP_BUTT,
    FT_STROKER_LINECAP_ROUND,
    FT_STROKER_LINECAP_SQUARE
    );

  FT_Stroker_LineJoin =
    (
    FT_STROKER_LINEJOIN_ROUND,
    FT_STROKER_LINEJOIN_BEVEL,
    FT_STROKER_LINEJOIN_MITE
    );

  FT_Open_Args = packed record
    flags: FT_UInt;
    memory_base: FT_Byte_ptr;
    memory_size: FT_Long;
    pathname: ^FT_String;
    stream: FT_Stream;
    driver: FT_Module;
    num_params: FT_Int;
    params: ^FT_Parameter;
  end;

  FT_Var_Axis = packed record
    name: ^FT_String;

    minimum: FT_Fixed;
    def: FT_Fixed;
    maximum: FT_Fixed;

    tag: FT_ULong;
    strid: FT_UInt;
  end;

  FT_Var_Named_Style = packed record
    coords: ^FT_Fixed;
    strid: FT_UInt;
  end;

  FT_MM_Var = packed record
    num_axis: FT_UInt;
    num_designs: FT_UInt;
    num_namedstyles: FT_UInt;
    axis: ^FT_Var_Axis;
    namedstyle: ^FT_Var_Named_Style;
  end;

  FT_Outline_MoveToFunc = function(const ato: FT_Vector;
    user: Pointer): Integer; cdecl;

  FT_Outline_LineToFunc = function(const ato: FT_Vector;
    user: Pointer): Integer; cdecl;

  FT_Outline_ConicToFunc = function(const control: FT_Vector;
    const ato: FT_Vector;
    user: Pointer): Integer; cdecl;

  FT_Outline_CubicToFunc = function(const control1: FT_Vector;
    const control2: FT_Vector;
    const ato: FT_Vector;
    user: Pointer): Integer; cdecl;

  FT_Outline_Funcs = packed record
    move_to: FT_Outline_MoveToFunc;
    line_to: FT_Outline_LineToFunc;
    conic_to: FT_Outline_ConicToFunc;
    cubic_to: FT_Outline_CubicToFunc;

    shift: Integer;
    delta: FT_Pos;
  end;

  FT_Tables = array[0..FT_VALIDATE_GX_LENGTH - 1] of FT_Bytes;

var
  FTC_CMapCache_Lookup: function(cache: FTC_CMapCache;
    face_id: FTC_FaceID;
    cmap_index: FT_Int;
    char_code: FT_UInt32): FT_UInt; cdecl;

  FTC_CMapCache_New: function(manager: FTC_Manager;
    var acache: FTC_ImageCache): FT_Error; cdecl;

  FTC_ImageCache_Lookup: function(cache: FTC_ImageCache;
    Atype: FTC_ImageType;
    gindex: FT_UInt;
    var aglyph: FT_Glyph;
    var anode: FTC_Node): FT_Error; cdecl;

  FTC_ImageCache_LookupScaler: function(cache: FTC_ImageCache;
    scaler: FTC_Scaler;
    load_flags: FT_ULong;
    gindex: FT_UInt;
    var aglyph: FT_Glyph;
    var anode: FTC_Node): FT_Error; cdecl;

  FTC_ImageCache_New: function(manager: FTC_Manager;
    var acache: FTC_ImageCache): FT_Error; cdecl;

  FTC_Manager_Done: procedure(manager: FTC_Manager); cdecl;

  FTC_Manager_LookupFace: function(manager: FTC_Manager;
    face_id: FTC_FaceID;
    var aface: FT_Face): FT_Error; cdecl;

  FTC_Manager_LookupSize: function(manager: FTC_Manager;
    scaler: FTC_Scaler;
    var asize: FT_Size): FT_Error; cdecl;

  FTC_Manager_New: function(Alibrary: FT_Library;
    max_faces: FT_UInt;
    max_sizes: FT_UInt;
    max_bytes: FT_ULong;
    requester: FTC_Face_Requester;
    req_data: FT_Pointer;
    var amanager: FTC_Manager): FT_Error; cdecl;

  FTC_Manager_RemoveFaceID: procedure(manager: FTC_Manager;
    face_id: FTC_FaceID); cdecl;

  FTC_Manager_Reset: procedure(manager: FTC_Manager); cdecl;

  FTC_Node_Unref: procedure(node: FTC_Node;
    manage: FTC_Manager); cdecl;

  FTC_SBitCache_Lookup: function(cache: FTC_SBitCache;
    Atype: FTC_ImageType;
    gindex: FT_UInt;
    var sbit: FTC_SBit;
    var anode: FTC_Node): FT_Error; cdecl;

  FTC_SBitCache_LookupScaler: function(cache: FTC_SBitCache;
    scaler: FTC_Scaler;
    load_flags: FT_ULong;
    gindex: FT_UInt;
    var sbit: FTC_SBit;
    var anode: FTC_Node): FT_Error; cdecl;

  FTC_SBitCache_New: function(manager: FTC_Manager;
    var acache: FTC_SBitCache): FT_Error; cdecl;

  FT_Activate_Size: function(size: FT_Size): FT_Error; cdecl;

  FT_Add_Default_Modules: function(Alibrary: FT_Library): FT_Error; cdecl;
  FT_Add_Module: function(Alibrary: FT_Library;
    const clazz: FT_Module_Class): FT_Error; cdecl;

  FT_Angle_Diff: function(angle1: FT_Angle;
    angle2: FT_Angle): FT_Angle; cdecl;

  FT_Atan2: function(x: FT_Fixed;
    y: FT_Fixed): FT_Angle; cdecl;

  FT_Attach_File: function(face: FT_Face; filepathname: PAnsiChar): FT_Error; cdecl;

  FT_Attach_Stream: function(face: FT_Face;
    const parameters: FT_Open_Args): FT_Error; cdecl;

  FT_Bitmap_Convert: function(Alibrary: FT_Library;
    const source: FT_Bitmap;
    var target: FT_Bitmap;
    alignment: FT_Int): FT_Error; cdecl;

  FT_Bitmap_Copy: function(Alibrary: FT_Library;
    const source: FT_Bitmap;
    var target: FT_Bitmap): FT_Error; cdecl;

  FT_Bitmap_Done: function(Alibrary: FT_Library;
    var bitmap: FT_Bitmap): FT_Error; cdecl;

  FT_Bitmap_Embolden: function(Alibrary: FT_Library;
    var bitmap: FT_Bitmap;
    xStrength: FT_Pos;
    yStrength: FT_Pos): FT_Error; cdecl;

  FT_Bitmap_New: procedure(var abitmap: FT_Bitmap); cdecl;

  FT_CeilFix: function(a: FT_Fixed): FT_Fixed; cdecl;

  FT_ClassicKern_Free: procedure(face: FT_Face;
    table: FT_Bytes); cdecl;

  FT_ClassicKern_Validate: function(face: FT_Face;
    validation_flags: FT_UInt;
    var ckern_table: FT_Bytes): FT_Error; cdecl;

  FT_Cos: function(angle: FT_Angle): FT_Fixed; cdecl;

  FT_DivFix: function(a: FT_Long;
    b: FT_Long): FT_Long; cdecl;

  FT_Done_Face: function(face: FT_Face): FT_Error; cdecl;

  FT_Done_FreeType: function(alibrary: FT_Library): FT_Error; cdecl;

  FT_Done_Glyph: procedure(glyph: FT_Glyph); cdecl;

  FT_Done_Library: function(Alibrary: FT_Library): FT_Error; cdecl;

  FT_Done_Size: function(size: FT_Size): FT_Error; cdecl;

  FT_Face_CheckTrueTypePatents: function(face: FT_Face): FT_Bool; cdecl;

  FT_Face_SetUnpatentedHinting: function(face: FT_Face;
    value: FT_Bool): FT_Bool; cdecl;

  FT_FloorFix: function(a: FT_Fixed): FT_Fixed; cdecl;

  FT_Get_CMap_Format: function(charmap: FT_CharMap): FT_Long; cdecl;

  FT_Get_CMap_Language_ID: function(charmap: FT_CharMap): FT_ULong; cdecl;

  FT_Get_Char_Index: function(face: FT_Face; charcode: FT_ULong): FT_UInt; cdecl;

  FT_Get_Charmap_Index: function(charmap: FT_CharMap): FT_Int; cdecl;

  FT_Get_First_Char: function(face: FT_Face;
    var agindex: FT_UInt): FT_ULong; cdecl;

  FT_Get_Glyph: function(slot: FT_GlyphSlot;
    var aglyph: FT_Glyph): FT_Error; cdecl;

  FT_Get_Glyph_Name: function(face: FT_Face;
    glyph_index: FT_UInt;
    buffer: FT_Pointer;
    buffer_max: FT_UInt): FT_Error; cdecl;

  FT_Get_Kerning: function(
    face: FT_Face;
    left_glyph, right_glyph, kern_mode: FT_UInt;
    var akerning: FT_Vector): FT_Error; cdecl;

  FT_Get_MM_Var: function(face: FT_Face;
    var amaster: FT_MM_Var): FT_Error; cdecl;

  FT_Get_Module: function(Alibrary: FT_Library;
    module_name: PAnsiChar): FT_Module; cdecl;

  FT_Get_Multi_Master: function(face: FT_Face;
    var amaster: FT_Multi_Master): FT_Error; cdecl;

  FT_Get_Name_Index: function(face: FT_Face;
    var glyph_name: FT_String): FT_UInt; cdecl;

  FT_Get_Next_Char: function(face: FT_Face;
    char_code: FT_ULong;
    var agindex: FT_UInt): FT_ULong; cdecl;

  FT_Get_PFR_Advance: function(face: FT_Face;
    gindex: FT_UInt;
    var aadvance: FT_Pos): FT_Error; cdecl;

  FT_Get_PFR_Kerning: function(face: FT_Face;
    left: FT_UInt;
    right: FT_UInt;
    var avector: FT_Vector): FT_Error; cdecl;

  FT_Get_PFR_Metrics: function(face: FT_Face;
    var aoutline_resolution: FT_UInt;
    var ametrics_resolution: FT_UInt;
    ametrics_x_scale: FT_Fixed;
    ametrics_y_scale: FT_Fixed): FT_Error; cdecl;

  FT_Get_PS_Font_Info: function(face: FT_Face;
    afont_info: PS_FontInfo): FT_Error; cdecl;

  FT_Get_PS_Font_Private: function(face: FT_Face;
    afont_private: PS_Private): FT_Error; cdecl;

  FT_Get_Postscript_Name: function(face: FT_Face): PAnsiChar; cdecl;

  FT_Get_Renderer: function(Alibrary: FT_Library;
    format: FT_Glyph_Format): FT_Renderer; cdecl;

  FT_Get_Sfnt_Name: function(face: FT_Face;
    idx: FT_UInt;
    var aname: FT_SfntName): FT_Error; cdecl;

  FT_Get_Sfnt_Name_Count: function(face: FT_Face): FT_UInt; cdecl;

  FT_Get_Sfnt_Table: function(face: FT_Face;
    tag: FT_Sfnt_Tag): Pointer; cdecl;

  FT_Get_SubGlyph_Info: function(glyph: FT_GlyphSlot;
    sub_index: FT_UInt;
    var p_index: FT_Int;
    var p_flags: FT_UInt;
    var p_arg1: FT_Int;
    var p_arg2: FT_Int;
    var p_transform: FT_Matrix): FT_Error; cdecl;

  FT_Get_Track_Kerning: function(face: FT_Face;
    point_size: FT_Fixed;
    degree: FT_Int;
    var akerning: FT_Fixed): FT_Error; cdecl;

  FT_Get_TrueType_Engine_Type: function(Alibrary: FT_Library): FT_TrueTypeEngineType; cdecl;

  FT_Get_WinFNT_Header: function(face: FT_Face;
    var aheader: FT_WinFNT_HeaderRec): FT_Error; cdecl;

  FT_GlyphSlot_Embolden: procedure(slot: FT_GlyphSlot); cdecl;

  FT_GlyphSlot_Oblique: procedure(slot: FT_GlyphSlot); cdecl;

  FT_GlyphSlot_Own_Bitmap: function(slot: FT_GlyphSlot): FT_Error; cdecl;

  FT_Glyph_Copy: function(source: FT_Glyph;
    var target: FT_Glyph): FT_Error; cdecl;

  FT_Glyph_Get_CBox: procedure(glyph: FT_Glyph;
    bbox_mode: FT_UInt;
    var acbo: FT_BBox); cdecl;

  FT_Glyph_Stroke: function(var pglyph: FT_Glyph;
    stroker: FT_Stroker;
    destroy: FT_Bool): FT_Error; cdecl;

  FT_Glyph_StrokeBorder: function(var pglyph: FT_Glyph;
    stroker: FT_Stroker;
    inside: FT_Bool;
    destroy: FT_Bool): FT_Error; cdecl;

  FT_Glyph_To_Bitmap: function(var the_glyph: FT_Glyph;
    render_mode: FT_Render_Mode;
    var origin: FT_Vector;
    destroy: FT_Bool): FT_Error; cdecl;

  FT_Glyph_Transform: function(glyph: FT_Glyph;
    var matrix: FT_Matrix;
    var delta: FT_Vector): FT_Error; cdecl;

  FT_Has_PS_Glyph_Names: function(face: FT_Face): FT_Int; cdecl;

  FT_Init_FreeType: function(alibrary: FT_Library): FT_Error; cdecl;

  FT_Library_Version: procedure(Alibrary: FT_Library;
    var amajor: FT_Int;
    var aminor: FT_Int;
    var apatch: FT_Int); cdecl;

  FT_List_Add: procedure(list: FT_List;
    node: FT_ListNode); cdecl;

  FT_List_Finalize: procedure(list: FT_List;
    destroy: FT_List_Destructor;
    memory: FT_Memory;
    user: Pointer); cdecl;

  FT_List_Find: function(list: FT_List;
    data: Pointer): FT_ListNode; cdecl;

  FT_List_Insert: procedure(list: FT_List;
    node: FT_ListNode); cdecl;

  FT_List_Iterate: function(list: FT_List;
    iterator: FT_List_Iterator;
    user: Pointer): FT_Error; cdecl;

  FT_List_Remove: procedure(list: FT_List;
    node: FT_ListNode); cdecl;

  FT_List_Up: procedure(list: FT_List;
    node: FT_ListNode); cdecl;

  FT_Load_Char: function(face: FT_Face;
    char_code: FT_ULong;
    load_flags: FT_Int32): FT_Error; cdecl;

  FT_Load_Glyph: function(
    face: FT_Face;
    glyph_index: FT_UInt;
    load_flags: FT_Int32): FT_Error; cdecl;

  FT_Load_Sfnt_Table: function(face: FT_Face;
    tag: FT_ULong;
    offset: FT_Long;
    var buffer: FT_Byte;
    var length: FT_ULong): FT_Error; cdecl;

  FT_Matrix_Invert: function(var matrix: FT_Matrix): FT_Error; cdecl;

  FT_Matrix_Multiply: procedure(var a: FT_Matrix;
    var b: FT_Matrix); cdecl;

  FT_MulDiv: function(a: FT_Long;
    b: FT_Long;
    c: FT_Long): FT_Long; cdecl;

  FT_MulFix: function(a: FT_Long;
    b: FT_Long): FT_Long; cdecl;

  FT_New_Face: function(
    library_: FT_Library;
    filepathname: PAnsiChar;
    face_index: FT_Long;
    var aface: FT_Face): FT_Error; cdecl;

  FT_New_Library: function(memory: FT_Memory;
    var alibrary: FT_Library): FT_Error; cdecl;

  FT_New_Memory_Face: function(
    library_: FT_Library;
    file_base: FT_Byte_ptr;
    file_size,
    face_index: FT_Long;
    var aface: FT_Face): FT_Error; cdecl;

  FT_New_Size: function(face: FT_Face;
    var size: FT_Size): FT_Error; cdecl;

  FT_OpenType_Free: procedure(face: FT_Face;
    table: FT_Bytes); cdecl;

  FT_OpenType_Validate: function(face: FT_Face;
    validation_flags: FT_UInt;
    var BASE_table: FT_Bytes;
    var GDEF_table: FT_Bytes;
    var GPOS_table: FT_Bytes;
    var GSUB_table: FT_Bytes;
    var JSTF_table: FT_Bytes): FT_Error; cdecl;

  FT_Open_Face: function(Alibrary: FT_Library;
    const args: FT_Open_Args;
    face_index: FT_Long;
    var aface: FT_Face): FT_Error; cdecl;

  FT_Outline_Check: function(var outline: FT_Outline): FT_Error; cdecl;

  FT_Outline_Copy: function(const source: FT_Outline;
    var target: FT_Outline): FT_Error; cdecl;

  FT_Outline_Decompose: function(var outline: FT_Outline;
    const func_interface: FT_Outline_Funcs;
    use: Pointer): FT_Error; cdecl;

  FT_Outline_Done: function(Alibrary: FT_Library;
    var outline: FT_Outline): FT_Error; cdecl;

  FT_Outline_Done_Internal: function(memory: FT_Memory;
    var outline: FT_Outline): FT_Error; cdecl;

  FT_Outline_Embolden: function(var outline: FT_Outline;
    strength: FT_Pos): FT_Error; cdecl;

  FT_Outline_GetInsideBorder: function(const outline: FT_Outline): FT_StrokerBorder; cdecl;

  FT_Outline_GetOutsideBorder: function(const outline: FT_Outline): FT_StrokerBorder; cdecl;

  FT_Outline_Get_BBox: function(const outline: FT_Outline;
    var abbox: FT_BBox): FT_Error; cdecl;

  FT_Outline_Get_Bitmap: function(Alibrary: FT_Library;
    const outline: FT_Outline;
    var abitmap: FT_Bitmap): FT_Error; cdecl;

  FT_Outline_Get_CBox: procedure(const outline: FT_Outline;
    out acbo: FT_BBox); cdecl;

  FT_Outline_Get_Orientation: function(const outline: FT_Outline): FT_Orientation; cdecl;

  FT_Outline_New: function(Alibrary: FT_Library;
    numPoints: FT_UInt;
    numContours: FT_Int;
    var anoutline: FT_Outline): FT_Error; cdecl;

  FT_Outline_New_Internal: function(memory: FT_Memory;
    numPoints: FT_UInt;
    numContours: FT_Int;
    var anoutline: FT_Outline): FT_Error; cdecl;

  FT_Outline_Render: function(Alibrary: FT_Library;
    var outline: FT_Outline;
    const params: FT_Raster_Params): FT_Error; cdecl;

  FT_Outline_Reverse: procedure(const outline: FT_Outline); cdecl;

  FT_Outline_Transform: procedure(var outline: FT_Outline;
    const matrix: FT_Matrix); cdecl;

  FT_Outline_Translate: procedure(var outline: FT_Outline;
    xOffset: FT_Pos;
    yOffset: FT_Pos); cdecl;

  FT_Remove_Module: function(Alibrary: FT_Library;
    module: FT_Module): FT_Error; cdecl;

  FT_Render_Glyph: function(slot: FT_GlyphSlot; render_mode: FT_Render_Mode): FT_Error; cdecl;

  FT_Request_Size: function(face: FT_Face;
    req: FT_Size_Request): FT_Error; cdecl;

  FT_RoundFix: function(a: FT_Fixed): FT_Fixed; cdecl;

  FT_Select_Charmap: function(face: FT_Face; encoding: FT_Encoding): FT_Error; cdecl;

  FT_Select_Size: function(face: FT_Face;
    strike_index: FT_Int): FT_Error; cdecl;

  FT_Set_Char_Size: function(
    face: FT_Face_ptr;
    char_width, char_height: FT_F26dot6;
    horz_res, vert_res: FT_UInt): FT_Error; cdecl;

  FT_Set_Charmap: function(face: FT_Face;
    charmap: FT_CharMap): FT_Error; cdecl;

  FT_Set_Debug_Hook: procedure(Alibrary: FT_Library;
    hook_index: FT_UInt;
    debug_hook: FT_DebugHook_Func); cdecl;

  FT_Set_MM_Blend_Coordinates: function(face: FT_Face;
    num_coords: FT_UInt;
    var coords: FT_Fixed): FT_Error; cdecl;

  FT_Set_MM_Design_Coordinates: function(face: FT_Face;
    num_coords: FT_UInt;
    coords: FT_Long_ptr): FT_Error; cdecl;

  FT_Set_Pixel_Sizes: function(
    face: FT_Face_ptr;
    pixel_width, pixel_height: FT_UInt): FT_Error; cdecl;

  FT_Set_Renderer: function(Alibrary: FT_Library;
    renderer: FT_Renderer;
    num_params: FT_UInt;
    const parameters: FT_Parameter): FT_Error; cdecl;

  FT_Set_Transform: procedure(face: FT_Face;
    var matrix: FT_Matrix;
    var delta: FT_Vector); cdecl;

  FT_Set_Var_Blend_Coordinates: function(face: FT_Face;
    num_coords: FT_UInt;
    coords: FT_Fixed_ptr): FT_Error; cdecl;

  FT_Set_Var_Design_Coordinates: function(face: FT_Face;
    num_coords: FT_UInt;
    coords: FT_Fixed_ptr): FT_Error; cdecl;

  FT_Sfnt_Table_Info: function(face: FT_Face;
    table_index: FT_UInt;
    var tag: FT_ULong;
    var length: FT_ULong): FT_Error; cdecl;

  FT_Sin: function(angle: FT_Angle): FT_Fixed; cdecl;

  FT_Stream_OpenGzip: function(stream: FT_Stream;
    sourc: FT_Stream): FT_Error; cdecl;

  FT_Stream_OpenLZW: function(stream: FT_Stream;
    sourc: FT_Stream): FT_Error; cdecl;

  FT_Stroker_BeginSubPath: function(stroker: FT_Stroker;
    const ato: FT_Vector;
    open: FT_Bool): FT_Error; cdecl;

  FT_Stroker_ConicTo: function(stroker: FT_Stroker;
    const control: FT_Vector;
    var t: FT_Vector): FT_Error; cdecl;

  FT_Stroker_CubicTo: function(stroker: FT_Stroker;
    const control1: FT_Vector;
    const control2: FT_Vector;
    var ato: FT_Vector): FT_Error; cdecl;

  FT_Stroker_Done: procedure(stroker: FT_Stroker); cdecl;

  FT_Stroker_EndSubPath: function(stroker: FT_Stroker): FT_Error; cdecl;

  FT_Stroker_Export: procedure(stroker: FT_Stroker;
    var outline: FT_Outline); cdecl;

  FT_Stroker_ExportBorder: procedure(stroker: FT_Stroker;
    border: FT_StrokerBorder;
    var outline: FT_Outline); cdecl;

  FT_Stroker_GetBorderCounts: function(stroker: FT_Stroker;
    border: FT_StrokerBorder;
    anum_points: FT_UInt_ptr;
    anum_contours: FT_UInt_ptr): FT_Error; cdecl;

  FT_Stroker_GetCounts: function(stroker: FT_Stroker;
    anum_points: FT_UInt_ptr;
    anum_contours: FT_UInt_ptr): FT_Error; cdecl;

  FT_Stroker_LineTo: function(stroker: FT_Stroker;
    var ato: FT_Vector): FT_Error; cdecl;

  FT_Stroker_New: function(Alibrary: FT_Library;
    var astroker: FT_Stroker): FT_Error; cdecl;

  FT_Stroker_ParseOutline: function(stroker: FT_Stroker;
    const outline: FT_Outline;
    opened: FT_Bool): FT_Error; cdecl;

  FT_Stroker_Rewind: procedure(stroker: FT_Stroker); cdecl;

  FT_Stroker_Set: procedure(stroker: FT_Stroker;
    radius: FT_Fixed;
    line_cap: FT_Stroker_LineCap;
    line_join: FT_Stroker_LineJoin;
    miter_limit: FT_Fixed); cdecl;

  FT_Tan: function(angle: FT_Angle): FT_Fixed; cdecl;

  FT_TrueTypeGX_Free: procedure(face: FT_Face;
    table: FT_Bytes); cdecl;

  FT_TrueTypeGX_Validate: function(face: FT_Face;
    validation_flags: FT_UInt;
    const tables: FT_Tables;
    table_length: FT_UInt): FT_Error; cdecl;

  FT_Vector_From_Polar: procedure(var vec: FT_Vector;
    length: FT_Fixed;
    angle: FT_Angle); cdecl;

  FT_Vector_Length: function(var vec: FT_Vector): FT_Fixed; cdecl;

  FT_Vector_Polarize: procedure(const vec: FT_Vector;
    var length: FT_Fixed;
    var angl: FT_Angle); cdecl;

  FT_Vector_Rotate: procedure(var vec: FT_Vector;
    angle: FT_Angle); cdecl;

  FT_Vector_Transform: procedure(var vec: FT_Vector;
    const matrix: FT_Matrix); cdecl;

  FT_Vector_Unit: procedure(var vec: FT_Vector;
    angle: FT_Angle); cdecl;

function FT_Curve_Tag(flag: char): char;
function FT_Is_Scalable(face: FT_Face): boolean;
function FT_Has_Kerning(face: FT_Face): boolean;

function InitFreetype: Boolean;
procedure CloseFreetype;
function InitFreetypeFromLibrary(const FTName: WideString): Boolean;
function IsFreetypeInitialized: Boolean;

implementation

function FT_CURVE_TAG(flag: char): char;
begin
  result := char(Byte(flag) and 3);
end;

function FT_IS_SCALABLE(face: FT_Face): boolean;
begin
  result := boolean(face.face_flags and FT_FACE_FLAG_SCALABLE);
end;

function FT_HAS_KERNING(face: FT_Face): boolean;
begin
  result := boolean(face.face_flags and FT_FACE_FLAG_KERNING);
end;

const
  INVALID_MODULEHANDLE = 0;

  // ************** Windows specific ********************
{$IFDEF MSWINDOWS}
var
  FTHandle: HINST;
{$ENDIF}
  // ************** UNIX specific ********************
{$IFDEF UNIX}
var
  FTHandle: TLibHandle;
{$ENDIF}

function FTGetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  result := GetProcAddress(Cardinal(FTHandle), ProcName);
end;

// InitFreetype
//

function InitFreetype: Boolean;
begin
  if FTHandle = INVALID_MODULEHANDLE then
    Result := InitFreetypeFromLibrary(FTLIBNAME)
  else
    Result := True;
end;

// CloseFreetype
//

procedure CloseFreetype;
begin
  if FTHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(Cardinal(FTHandle));
    FTHandle := INVALID_MODULEHANDLE;
  end;
end;

// InitFreetypeFromLibrary
//

function InitFreetypeFromLibrary(const FTName: WideString): Boolean;
begin
  Result := False;
  CloseFreetype;
  FTHandle := LoadLibraryW(PWideChar(FTName));
  if FTHandle = INVALID_MODULEHANDLE then
    Exit;

  FTC_CMapCache_Lookup := FTGetProcAddress('FTC_CMapCache_Lookup');
  FTC_CMapCache_New := FTGetProcAddress('FTC_CMapCache_New');
  FTC_ImageCache_Lookup := FTGetProcAddress('FTC_ImageCache_Lookup');
  FTC_ImageCache_LookupScaler := FTGetProcAddress('FTC_ImageCache_LookupScaler');
  FTC_ImageCache_New := FTGetProcAddress('FTC_ImageCache_New');
  FTC_Manager_Done := FTGetProcAddress('FTC_Manager_Done');
  FTC_Manager_LookupFace := FTGetProcAddress('FTC_Manager_LookupFace');
  FTC_Manager_LookupSize := FTGetProcAddress('FTC_Manager_LookupSize');
  FTC_Manager_New := FTGetProcAddress('FTC_Manager_New');
  FTC_Manager_RemoveFaceID := FTGetProcAddress('FTC_Manager_RemoveFaceID');
  FTC_Manager_Reset := FTGetProcAddress('FTC_Manager_Reset');
  FTC_Node_Unref := FTGetProcAddress('FTC_Node_Unref');
  FTC_SBitCache_Lookup := FTGetProcAddress('FTC_SBitCache_Lookup');
  FTC_SBitCache_LookupScaler := FTGetProcAddress('FTC_SBitCache_LookupScaler');
  FTC_SBitCache_New := FTGetProcAddress('FTC_SBitCache_New');
  FT_Activate_Size := FTGetProcAddress('FT_Activate_Size');
  FT_Add_Default_Modules := FTGetProcAddress('FT_Add_Default_Modules');
  FT_Add_Module := FTGetProcAddress('FT_Add_Module');
  FT_Angle_Diff := FTGetProcAddress('FT_Angle_Diff');
  FT_Atan2 := FTGetProcAddress('FT_Atan2');
  FT_Attach_File := FTGetProcAddress('FT_Attach_File');
  FT_Attach_Stream := FTGetProcAddress('FT_Attach_Stream');
  FT_Bitmap_Convert := FTGetProcAddress('FT_Bitmap_Convert');
  FT_Bitmap_Copy := FTGetProcAddress('FT_Bitmap_Copy');
  FT_Bitmap_Done := FTGetProcAddress('FT_Bitmap_Done');
  FT_Bitmap_Embolden := FTGetProcAddress('FT_Bitmap_Embolden');
  FT_Bitmap_New := FTGetProcAddress('FT_Bitmap_New');
  FT_CeilFix := FTGetProcAddress('FT_CeilFix');
  FT_ClassicKern_Free := FTGetProcAddress('FT_ClassicKern_Free');
  FT_ClassicKern_Validate := FTGetProcAddress('FT_ClassicKern_Validate');
  FT_Cos := FTGetProcAddress('FT_Cos');
  FT_DivFix := FTGetProcAddress('FT_DivFix');
  FT_Done_Face := FTGetProcAddress('FT_Done_Face');
  FT_Done_FreeType := FTGetProcAddress('FT_Done_FreeType');
  FT_Done_Glyph := FTGetProcAddress('FT_Done_Glyph');
  FT_Done_Library := FTGetProcAddress('FT_Done_Library');
  FT_Done_Size := FTGetProcAddress('FT_Done_Size');
  FT_Face_CheckTrueTypePatents := FTGetProcAddress('FT_Face_CheckTrueTypePatents');
  FT_Face_SetUnpatentedHinting := FTGetProcAddress('FT_Face_SetUnpatentedHinting');
  FT_FloorFix := FTGetProcAddress('FT_FloorFix');
  FT_Get_CMap_Format := FTGetProcAddress('FT_Get_CMap_Format');
  FT_Get_CMap_Language_ID := FTGetProcAddress('FT_Get_CMap_Language_ID');
  FT_Get_Char_Index := FTGetProcAddress('FT_Get_Char_Index');
  FT_Get_Charmap_Index := FTGetProcAddress('FT_Get_Charmap_Index');
  FT_Get_First_Char := FTGetProcAddress('FT_Get_First_Char');
  FT_Get_Glyph := FTGetProcAddress('FT_Get_Glyph');
  FT_Get_Glyph_Name := FTGetProcAddress('FT_Get_Glyph_Name');
  FT_Get_Kerning := FTGetProcAddress('FT_Get_Kerning');
  FT_Get_MM_Var := FTGetProcAddress('FT_Get_MM_Var');
  FT_Get_Module := FTGetProcAddress('FT_Get_Module');
  FT_Get_Multi_Master := FTGetProcAddress('FT_Get_Multi_Master');
  FT_Get_Name_Index := FTGetProcAddress('FT_Get_Name_Index');
  FT_Get_Next_Char := FTGetProcAddress('FT_Get_Next_Char');
  FT_Get_PFR_Advance := FTGetProcAddress('FT_Get_PFR_Advance');
  FT_Get_PFR_Kerning := FTGetProcAddress('FT_Get_PFR_Kerning');
  FT_Get_PFR_Metrics := FTGetProcAddress('FT_Get_PFR_Metrics');
  FT_Get_PS_Font_Info := FTGetProcAddress('FT_Get_PS_Font_Info');
  FT_Get_PS_Font_Private := FTGetProcAddress('FT_Get_PS_Font_Private');
  FT_Get_Postscript_Name := FTGetProcAddress('FT_Get_Postscript_Name');
  FT_Get_Renderer := FTGetProcAddress('FT_Get_Renderer');
  FT_Get_Sfnt_Name := FTGetProcAddress('FT_Get_Sfnt_Name');
  FT_Get_Sfnt_Name_Count := FTGetProcAddress('FT_Get_Sfnt_Name_Count');
  FT_Get_Sfnt_Table := FTGetProcAddress('FT_Get_Sfnt_Table');
  FT_Get_SubGlyph_Info := FTGetProcAddress('FT_Get_SubGlyph_Info');
  FT_Get_Track_Kerning := FTGetProcAddress('FT_Get_Track_Kerning');
  FT_Get_TrueType_Engine_Type := FTGetProcAddress('FT_Get_TrueType_Engine_Type');
  FT_Get_WinFNT_Header := FTGetProcAddress('FT_Get_WinFNT_Header');
  FT_GlyphSlot_Embolden := FTGetProcAddress('FT_GlyphSlot_Embolden');
  FT_GlyphSlot_Oblique := FTGetProcAddress('FT_GlyphSlot_Oblique');
  FT_GlyphSlot_Own_Bitmap := FTGetProcAddress('FT_GlyphSlot_Own_Bitmap');
  FT_Glyph_Copy := FTGetProcAddress('FT_Glyph_Copy');
  FT_Glyph_Get_CBox := FTGetProcAddress('FT_Glyph_Get_CBox');
  FT_Glyph_Stroke := FTGetProcAddress('FT_Glyph_Stroke');
  FT_Glyph_StrokeBorder := FTGetProcAddress('FT_Glyph_StrokeBorder');
  FT_Glyph_To_Bitmap := FTGetProcAddress('FT_Glyph_To_Bitmap');
  FT_Glyph_Transform := FTGetProcAddress('FT_Glyph_Transform');
  FT_Has_PS_Glyph_Names := FTGetProcAddress('FT_Has_PS_Glyph_Names');
  FT_Init_FreeType := FTGetProcAddress('FT_Init_FreeType');
  FT_Library_Version := FTGetProcAddress('FT_Library_Version');
  FT_List_Add := FTGetProcAddress('FT_List_Add');
  FT_List_Finalize := FTGetProcAddress('FT_List_Finalize');
  FT_List_Find := FTGetProcAddress('FT_List_Find');
  FT_List_Insert := FTGetProcAddress('FT_List_Insert');
  FT_List_Iterate := FTGetProcAddress('FT_List_Iterate');
  FT_List_Remove := FTGetProcAddress('FT_List_Remove');
  FT_List_Up := FTGetProcAddress('FT_List_Up');
  FT_Load_Char := FTGetProcAddress('FT_Load_Char');
  FT_Load_Glyph := FTGetProcAddress('FT_Load_Glyph');
  FT_Load_Sfnt_Table := FTGetProcAddress('FT_Load_Sfnt_Table');
  FT_Matrix_Invert := FTGetProcAddress('FT_Matrix_Invert');
  FT_Matrix_Multiply := FTGetProcAddress('FT_Matrix_Multiply');
  FT_MulDiv := FTGetProcAddress('FT_MulDiv');
  FT_MulFix := FTGetProcAddress('FT_MulFix');
  FT_New_Face := FTGetProcAddress('FT_New_Face');
  FT_New_Library := FTGetProcAddress('FT_New_Library');
  FT_New_Memory_Face := FTGetProcAddress('FT_New_Memory_Face');
  FT_New_Size := FTGetProcAddress('FT_New_Size');
  FT_OpenType_Free := FTGetProcAddress('FT_OpenType_Free');
  FT_OpenType_Validate := FTGetProcAddress('FT_OpenType_Validate');
  FT_Open_Face := FTGetProcAddress('FT_Open_Face');
  FT_Outline_Check := FTGetProcAddress('FT_Outline_Check');
  FT_Outline_Copy := FTGetProcAddress('FT_Outline_Copy');
  FT_Outline_Decompose := FTGetProcAddress('FT_Outline_Decompose');
  FT_Outline_Done := FTGetProcAddress('FT_Outline_Done');
  FT_Outline_Done_Internal := FTGetProcAddress('FT_Outline_Done_Internal');
  FT_Outline_Embolden := FTGetProcAddress('FT_Outline_Embolden');
  FT_Outline_GetInsideBorder := FTGetProcAddress('FT_Outline_GetInsideBorder');
  FT_Outline_GetOutsideBorder := FTGetProcAddress('FT_Outline_GetOutsideBorder');
  FT_Outline_Get_BBox := FTGetProcAddress('FT_Outline_Get_BBox');
  FT_Outline_Get_Bitmap := FTGetProcAddress('FT_Outline_Get_Bitmap');
  FT_Outline_Get_CBox := FTGetProcAddress('FT_Outline_Get_CBox');
  FT_Outline_Get_Orientation := FTGetProcAddress('FT_Outline_Get_Orientation');
  FT_Outline_New := FTGetProcAddress('FT_Outline_New');
  FT_Outline_New_Internal := FTGetProcAddress('FT_Outline_New_Internal');
  FT_Outline_Render := FTGetProcAddress('FT_Outline_Render');
  FT_Outline_Reverse := FTGetProcAddress('FT_Outline_Reverse');
  FT_Outline_Transform := FTGetProcAddress('FT_Outline_Transform');
  FT_Outline_Translate := FTGetProcAddress('FT_Outline_Translate');
  FT_Remove_Module := FTGetProcAddress('FT_Remove_Module');
  FT_Render_Glyph := FTGetProcAddress('FT_Render_Glyph');
  FT_Request_Size := FTGetProcAddress('FT_Request_Size');
  FT_RoundFix := FTGetProcAddress('FT_RoundFix');
  FT_Select_Charmap := FTGetProcAddress('FT_Select_Charmap');
  FT_Select_Size := FTGetProcAddress('FT_Select_Size');
  FT_Set_Char_Size := FTGetProcAddress('FT_Set_Char_Size');
  FT_Set_Charmap := FTGetProcAddress('FT_Set_Charmap');
  FT_Set_Debug_Hook := FTGetProcAddress('FT_Set_Debug_Hook');
  FT_Set_MM_Blend_Coordinates := FTGetProcAddress('FT_Set_MM_Blend_Coordinates');
  FT_Set_MM_Design_Coordinates := FTGetProcAddress('FT_Set_MM_Design_Coordinates');
  FT_Set_Pixel_Sizes := FTGetProcAddress('FT_Set_Pixel_Sizes');
  FT_Set_Renderer := FTGetProcAddress('FT_Set_Renderer');
  FT_Set_Transform := FTGetProcAddress('FT_Set_Transform');
  FT_Set_Var_Blend_Coordinates := FTGetProcAddress('FT_Set_Var_Blend_Coordinates');
  FT_Set_Var_Design_Coordinates := FTGetProcAddress('FT_Set_Var_Design_Coordinates');
  FT_Sfnt_Table_Info := FTGetProcAddress('FT_Sfnt_Table_Info');
  FT_Sin := FTGetProcAddress('FT_Sin');
  FT_Stream_OpenGzip := FTGetProcAddress('FT_Stream_OpenGzip');
  FT_Stream_OpenLZW := FTGetProcAddress('FT_Stream_OpenLZW');
  FT_Stroker_BeginSubPath := FTGetProcAddress('FT_Stroker_BeginSubPath');
  FT_Stroker_ConicTo := FTGetProcAddress('FT_Stroker_ConicTo');
  FT_Stroker_CubicTo := FTGetProcAddress('FT_Stroker_CubicTo');
  FT_Stroker_Done := FTGetProcAddress('FT_Stroker_Done');
  FT_Stroker_EndSubPath := FTGetProcAddress('FT_Stroker_EndSubPath');
  FT_Stroker_Export := FTGetProcAddress('FT_Stroker_Export');
  FT_Stroker_ExportBorder := FTGetProcAddress('FT_Stroker_ExportBorder');
  FT_Stroker_GetBorderCounts := FTGetProcAddress('FT_Stroker_GetBorderCounts');
  FT_Stroker_GetCounts := FTGetProcAddress('FT_Stroker_GetCounts');
  FT_Stroker_LineTo := FTGetProcAddress('FT_Stroker_LineTo');
  FT_Stroker_New := FTGetProcAddress('FT_Stroker_New');
  FT_Stroker_ParseOutline := FTGetProcAddress('FT_Stroker_ParseOutline');
  FT_Stroker_Rewind := FTGetProcAddress('FT_Stroker_Rewind');
  FT_Stroker_Set := FTGetProcAddress('FT_Stroker_Set');
  FT_Tan := FTGetProcAddress('FT_Tan');
  FT_TrueTypeGX_Free := FTGetProcAddress('FT_TrueTypeGX_Free');
  FT_TrueTypeGX_Validate := FTGetProcAddress('FT_TrueTypeGX_Validate');
  FT_Vector_From_Polar := FTGetProcAddress('FT_Vector_From_Polar');
  FT_Vector_Length := FTGetProcAddress('FT_Vector_Length');
  FT_Vector_Polarize := FTGetProcAddress('FT_Vector_Polarize');
  FT_Vector_Rotate := FTGetProcAddress('FT_Vector_Rotate');
  FT_Vector_Transform := FTGetProcAddress('FT_Vector_Transform');
  FT_Vector_Unit := FTGetProcAddress('FT_Vector_Unit');

  Result := True;
end;

// IsFreetypeInitialized
//

function IsFreetypeInitialized: Boolean;
begin
  Result := (FTHandle <> INVALID_MODULEHANDLE);
end;

initialization

finalization

  CloseFreetype;

end.

