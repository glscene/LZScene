//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Conversion of OpenCL header file: cl_platform.h to Delphi,
  from http://www.khronos.org/registry/cl/.

   History :  
   01/11/09 - DanB - Creation
   
}
// *****************************************************************************
// * Copyright (c) 2008-2009 The Khronos Group Inc.
// *
// * Permission is hereby granted, free of charge, to any person obtaining a
// * copy of this software and/or associated documentation files (the
// * "Materials"), to deal in the Materials without restriction, including
// * without limitation the rights to use, copy, modify, merge, publish,
// * distribute, sublicense, and/or sell copies of the Materials, and to
// * permit persons to whom the Materials are furnished to do so, subject to
// * the following conditions:
// *
// * The above copyright notice and this permission notice shall be included
// * in all copies or substantial portions of the Materials.
// *
// * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
// ****************************************************************************/

unit GLSCLPlatform;

interface

{$I GLScene.inc}

type

  TSize_T = SizeInt;
  Psize_t = PSizeInt;
  intptr_t = PtrInt;

  // Pintptr_t = ^intptr_t;

  // scalar types
  Tcl_char = ShortInt;
  Tcl_uchar = Byte;
  Tcl_short = SmallInt;
  Tcl_ushort = Word;
  Tcl_int = LongInt;
  Tcl_uint = LongWord;
  Tcl_long = Int64;
  Tcl_ulong = UInt64;

  Tcl_half = Word; // hmmm, no half precision floating point in Delphi
  Tcl_float = Single;
  Tcl_double = Double;

  Pcl_char = ^Tcl_char;
  Pcl_uchar = ^Tcl_uchar;
  Pcl_short = ^Tcl_short;
  Pcl_ushort = ^Tcl_ushort;
  Pcl_int = ^Tcl_int;
  Pcl_uint = ^Tcl_uint;
  Pcl_long = ^Tcl_long;
  Pcl_ulong = ^Tcl_ulong;

  Pcl_half = ^Tcl_half;
  Pcl_float = ^Tcl_float;
  Pcl_double = ^Tcl_double;

  // and a few goodies to go with them
const

  CL_CHAR_BIT = 8;
  CL_SCHAR_MAX = 127;
  CL_SCHAR_MIN = (-127 - 1);
  CL_CHAR_MAX = CL_SCHAR_MAX;
  CL_CHAR_MIN = CL_SCHAR_MIN;
  CL_UCHAR_MAX = 255;
  CL_SHRT_MAX = 32767;
  CL_SHRT_MIN = (-32767 - 1);
  CL_USHRT_MAX = 65535;
  CL_INT_MAX = 2147483647;
  CL_INT_MIN = (-2147483647 - 1);
  CL_UINT_MAX = $FFFFFFFF;
  CL_LONG_MAX = $7FFFFFFFFFFFFFFF;
  CL_LONG_MIN = -$7FFFFFFFFFFFFFFF - 1;
  CL_ULONG_MAX = $FFFFFFFFFFFFFFFF;

  CL_FLT_DIG = 6;
  CL_FLT_MANT_DIG = 24;
  CL_FLT_MAX_10_EXP = +38;
  CL_FLT_MAX_EXP = +128;
  CL_FLT_MIN_10_EXP = -37;
  CL_FLT_MIN_EXP = -125;
  CL_FLT_RADIX = 2;
  CL_FLT_MAX = 1.7E38; // 0x1.fffffep127f;
  CL_FLT_MIN = 1.17E-38; // 0x1.0p-126f;
  CL_FLT_EPSILON = 1.0E-7; // 0x1.0p-23f;

  CL_DBL_DIG = 15;
  CL_DBL_MANT_DIG = 53;
  CL_DBL_MAX_10_EXP = +308;
  CL_DBL_MAX_EXP = +1024;
  CL_DBL_MIN_10_EXP = -307;
  CL_DBL_MIN_EXP = -1021;
  CL_DBL_RADIX = 2;
  CL_DBL_MAX = 8.98E307; // 0x1.fffffffffffffp1023;
  CL_DBL_MIN = 2.2E-308; // 0x1.0p-1022;
  CL_DBL_EPSILON = 2.2E-26; // 0x1.0p-52;

  /// *
  // * Vector types
  // *
  // *  Note:   OpenCL requires that all types be naturally aligned.
  // *          This means that vector types must be naturally aligned.
  // *          For example, a vector of four floats must be aligned to
  // *          a 16 byte boundary (calculated as 4 * the natural 4-byte
  // *          alignment of the float).  The alignment qualifiers here
  // *          will only function properly if your compiler supports them
  // *          and if you don't actively work to defeat them.  For example,
  // *          in order for a cl_float4 to be 16 byte aligned in a struct,
  // *          the start of the struct must itself be 16-byte aligned.
  // *
  // *          Maintaining proper alignment is the user's responsibility.
  // */

type
  Tcl_char2 = array [0 .. 1] of Tcl_char;
  Tcl_char4 = array [0 .. 3] of Tcl_char;
  Tcl_char8 = array [0 .. 7] of Tcl_char;
  Tcl_char16 = array [0 .. 15] of Tcl_char;

  Tcl_uchar2 = array [0 .. 1] of Tcl_uchar;
  Tcl_uchar4 = array [0 .. 3] of Tcl_uchar;
  Tcl_uchar8 = array [0 .. 7] of Tcl_uchar;
  Tcl_uchar16 = array [0 .. 15] of Tcl_uchar;

  Tcl_short2 = array [0 .. 1] of Tcl_short;
  Tcl_short4 = array [0 .. 3] of Tcl_short;
  Tcl_short8 = array [0 .. 7] of Tcl_short;
  Tcl_short16 = array [0 .. 15] of Tcl_short;

  Tcl_ushort2 = array [0 .. 1] of Tcl_ushort;
  Tcl_ushort4 = array [0 .. 3] of Tcl_ushort;
  Tcl_ushort8 = array [0 .. 7] of Tcl_ushort;
  Tcl_ushort16 = array [0 .. 15] of Tcl_ushort;

  Tcl_int2 = array [0 .. 1] of Tcl_int;
  Tcl_int4 = array [0 .. 3] of Tcl_int;
  Tcl_int8 = array [0 .. 7] of Tcl_int;
  Tcl_int16 = array [0 .. 15] of Tcl_int;

  Tcl_uint2 = array [0 .. 1] of Tcl_uint;
  Tcl_uint4 = array [0 .. 3] of Tcl_uint;
  Tcl_uint8 = array [0 .. 7] of Tcl_uint;
  Tcl_uint16 = array [0 .. 15] of Tcl_uint;

  Tcl_long2 = array [0 .. 1] of Tcl_long;
  Tcl_long4 = array [0 .. 3] of Tcl_long;
  Tcl_long8 = array [0 .. 7] of Tcl_long;
  Tcl_long16 = array [0 .. 15] of Tcl_long;

  Tcl_ulong2 = array [0 .. 1] of Tcl_ulong;
  Tcl_ulong4 = array [0 .. 3] of Tcl_ulong;
  Tcl_ulong8 = array [0 .. 7] of Tcl_ulong;
  Tcl_ulong16 = array [0 .. 15] of Tcl_ulong;

  Tcl_float2 = array [0 .. 1] of Tcl_float;
  Tcl_float4 = array [0 .. 3] of Tcl_float;
  Tcl_float8 = array [0 .. 7] of Tcl_float;
  Tcl_float16 = array [0 .. 15] of Tcl_float;

  Tcl_double2 = array [0 .. 1] of Tcl_double;
  Tcl_double4 = array [0 .. 3] of Tcl_double;
  Tcl_double8 = array [0 .. 7] of Tcl_double;
  Tcl_double16 = array [0 .. 15] of Tcl_double;
  // There are no vector types for half

implementation

end.
