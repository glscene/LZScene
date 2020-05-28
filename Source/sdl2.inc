{*******************************************************************************

  SDL2.pas        v1.0  29/07/2013 first version for DelphiXE
                  v1.1  27/08/2013 add MACOS compability

  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

  Pascal-Header-Conversion SDL from the JEDI-Team written by Domenique Louis and others.
  Pascal-Header-Conversion SDL2 from the Copyright (C) 2012/13 Tim Blume aka End.

  convert SDL/SDL2 to SDL2 for DelphiXE by Kotai 2013  www.remakesonline.com

  The initial developer of this Pascal code was :
  Dominqiue Louis <Dominique@SavageSoftware.com.au>


*******************************************************************************}


{$define Debug}           { uncomment for debugging }
{$define no_smpeg}



{$IFNDEF FPC}
  {$IFDEF Debug}
    {$F+,D+,Q-,L+,R+,I-,S+,Y+,A+}
  {$ELSE}
    {$F+,Q-,R-,S-,I-,A+}
  {$ENDIF}
{$ELSE}  {FPC}
  //{$M+}
{$ENDIF} {FPC}


{$IFDEF LINUX}
{$DEFINE UNIX}
{$ENDIF}


{$IFDEF ver70}
   {$IFDEF Windows}
     {$DEFINE Win16}
   {$ENDIF Windows}
   {$IFDEF MSDOS}
     {$DEFINE NO_EXPORTS}
   {$ENDIF MSDOS}
   {$IFDEF DPMI}
     {$DEFINE BP_DPMI}
   {$ENDIF}
   {$DEFINE OS_16_BIT}
   {$DEFINE __OS_DOS__}
{$ENDIF ver70}


{$IFDEF ver80}
   {$DEFINE Delphi}      {Delphi 1.x}
   {$DEFINE Delphi16}
   {$DEFINE Win16}
   {$DEFINE OS_16_BIT}
   {$DEFINE __OS_DOS__}
{$ENDIF ver80}


{$IFDEF ver90}
   {$DEFINE Delphi}      {Delphi 2.x}
   {$DEFINE WIN32}
   {$DEFINE WINDOWS}
{$ENDIF ver90}


{$IFDEF ver100}
   {$DEFINE Delphi}      {Delphi 3.x}
   {$DEFINE WIN32}
   {$DEFINE WINDOWS}
{$ENDIF ver100}


{$IFDEF ver93}
   {$DEFINE Delphi}      {C++ Builder 1.x}
   {$DEFINE WINDOWS}
{$ENDIF ver93}


{$IFDEF ver110}
   {$DEFINE Delphi}      {C++ Builder 3.x}
   {$DEFINE WINDOWS}
{$ENDIF ver110}


{$IFDEF ver120}
   {$DEFINE Delphi}      {Delphi 4.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Has_Int64}
   {$DEFINE WINDOWS}
{$ENDIF ver120}


{$IFDEF ver130}
   {$DEFINE Delphi}      {Delphi / C++ Builder 5.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Has_Int64}
   {$DEFINE WINDOWS}
{$ENDIF ver130}


{$IFDEF ver140}
   {$DEFINE Delphi}      {Delphi / C++ Builder 6.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver140}


{$IFDEF ver150}
   {$DEFINE Delphi}      {Delphi 7.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver150}


{$IFDEF ver160}
   {$DEFINE Delphi}      {Delphi 8.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver160}


{$IFDEF ver170}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2005}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver170}


{$IFDEF ver180}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2006 / 2007}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver180}


{$IFDEF ver185}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2007}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver185}


{$IFDEF ver190}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2007 }
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver190}


{$IFDEF ver200}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2009 }
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver200}


{$IFDEF ver210}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2010}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver210}


{$IFDEF ver220}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver220}


{$IFDEF ver230}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE2}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver230}


{$IFDEF ver240}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE3}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$DEFINE Delphi17UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver240}


{$IFDEF ver250}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE4}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$DEFINE Delphi17UP}
   {$DEFINE Delphi18UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver250}


{$IFDEF UNIX}
  {$ifdef VER140}    // Kylix 1 & 2
    {$DEFINE KYLIX}
    {$DEFINE KYLIX1UP}
    {$DEFINE KYLIX2UP}
    {$DEFINE HAS_TYPES}
  {$endif}


  {$ifdef VER150}   // Kylix 3
    {$DEFINE KYLIX}
    {$DEFINE KYLIX1UP}
    {$DEFINE KYLIX2UP}
    {$DEFINE KYLIX3UP}
    {$DEFINE HAS_TYPES}
  {$endif}
{$ENDIF UNIX}


{$IFDEF VirtualPascal} {  Virtual Pascal 2.x }
   {$DEFINE Delphi}    {  Use Delphi Syntax }
   {$DEFINE VP2}
   {&Delphi+}
{$ENDIF VirtualPascal}


{$IFDEF Delphi}
  {$IFDEF MSWINDOWS}
     {$DEFINE Windows}
  {$ENDIF MSWINDOWS}
  {$DEFINE USE_STDCALL}
  {$IFDEF WIN32}
    {$DEFINE DELPHI32}
  {$ELSE}
    {$IFDEF WIN64}
	  {$DEFINE DELPHI64}
	{$ELSE}
	  {$DEFINE DELPHI16}
	{$ENDIF}
  {$ENDIF}
  //{$ALIGN ON}
{$ENDIF Delphi}


{$IFDEF FPC}
  {$MODE Delphi}          { use Delphi compatibility mode }
  {$H+}
  {$PACKRECORDS C}        // Added for record
  {$MACRO ON}             // Added For OpenGL
  {$DEFINE Delphi}
  {$DEFINE UseAT}
  {$UNDEF USE_STDCALL}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE Has_UInt64}
  {$DEFINE Has_Int64}
  {$DEFINE Has_Native}
  {$DEFINE NOCRT}
  {$IFDEF UNIX}
     {$DEFINE fpc_unix}
  {$ELSE}
     {$DEFINE __OS_DOS__}
  {$ENDIF}
  {$IFDEF WIN32}
   {$DEFINE UseWin}
  {$ENDIF}
  {$DEFINE HAS_TYPES}
{$ENDIF FPC}


{$IFDEF Win16}
  {$K+}   {smart callbacks}
{$ENDIF Win16}


 {$IFDEF OS2}
    {$UNDEF Windows}
    {$DEFINE UseWin}
    {$DEFINE OS_BigMem}
 {$ENDIF OS2}


{$IFDEF __TMT__}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE __OS_DOS__}
  {$DEFINE UseAT}
  {$IFNDEF MSDOS}
    {$DEFINE USE_STDCALL}
  {$ENDIF}


  {$IFDEF __WIN32__}
    {$DEFINE Win32}
    {$DEFINE UseWin}
    {$DEFINE NOCRT}
    {$DEFINE Win32}
    {$IFNDEF __CON__}
      {$DEFINE Windows}
    {$ENDIF}
  {$ENDIF}


  {$A+}  // Word alignment data
  {$OA+} // Objects and structures align
{$ENDIF}


{$IFDEF Win32}
  {$DEFINE OS_BigMem}
{$ELSE Win32}
  {$IFDEF ver70}
    {$DEFINE assembler}
  {$ENDIF} { use 16-bit assembler! }
{$ENDIF Win32}


{ ************************** dos/dos-like platforms **************}
{$IFDEF Windows}
   {$DEFINE __OS_DOS__}
   {$DEFINE UseWin}
//   {$DEFINE MSWINDOWS}
{$ENDIF Delphi}


{$IFDEF OS2}
   {$DEFINE __OS_DOS__}
   {$DEFINE Can_Use_DLL}
{$ENDIF Delphi}


{$IFDEF UseWin}
   {$DEFINE Can_Use_DLL}
{$ENDIF}


{$IFDEF Win16}
   {$DEFINE Can_Use_DLL}
{$ENDIF}


{$IFDEF BP_DPMI}
   {$DEFINE Can_Use_DLL}
{$ENDIF}


{$IFDEF USE_STDCALL}
   {$IFNDEF __TMT__}
     {$DEFINE BY_NAME}
   {$ENDIF}
{$ENDIF}


{$IFNDEF ver70}
  {$UNDEF assembler}
{$ENDIF}


{*************** define LITTLE ENDIAN platforms ********************}




{$IFDEF Delphi}
{$DEFINE IA32}
{$ENDIF}


{$IFDEF KYLIX}
{$DEFINE IA32}
{$ENDIF}


{$IFDEF FPC}
{$IFDEF FPC_LITTLE_ENDIAN}
{$DEFINE IA32}
{$ENDIF}
{$ENDIF}


{*************** define 16/32/64 Bit ********************}


{$IFDEF WINDOWS}
  {$IFDEF WIN16}
    {$DEFINE 16BIT}
  {$ENDIF}
  {$IFDEF WIN32}
    {$DEFINE 32BIT}
  {$ENDIF}
  {$IFDEF WIN64}
    {$DEFINE 64BIT}
  {$ENDIF}
  {$ELSE}
    //TODO!!
    {$DEFINE 32BIT}
  {$ENDIF}

